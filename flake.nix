{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    devenv.url = "github:kadena-io/devenv/devnet-setup";
    chainweb-node.url = "github:kadena-io/chainweb-node/edmund/fast-devnet";
    chainweb-node-l2.url = "github:kadena-io/chainweb-node/edmund/l2-spv-poc";
    chainweb-data = {
      url = "github:kadena-io/chainweb-data";
      inputs.nixpkgs.follows = "chainweb-node/nixpkgs";
      inputs.haskellNix.follows = "chainweb-node/haskellNix";
    };
    chainweb-mining-client = {
      url = "github:kadena-io/chainweb-mining-client/enis/update-to-flakes-and-haskellNix";
      inputs.haskellNix.follows = "chainweb-node/haskellNix";
      inputs.nixpkgs.follows = "chainweb-node/nixpkgs";
    };
    nix-exe-bundle = { url = "github:3noch/nix-bundle-exe"; flake = false; };
  };

  nixConfig = {
    extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs = { self
            , nixpkgs
            , devenv
            , systems
            , ... } @ inputs:
    let
      forEachSystem = nixpkgs.lib.genAttrs (import systems);
    in
    rec {
      packages = forEachSystem
        (system: rec {
          default = mkDevnetRunner {
            system = system;
            devnet = devShells.${system}.default;
          };
          l2 = mkDevnetRunner {
            system = system;
            devnet = devShells.${system}.l2;
          };
          container = mkContainer {
            system = system;
            devnetRunner = default;
          };
          container-l2 = mkContainer {
            system = system;
            devnetRunner = l2;
          };
          cwd = inputs.chainweb-data.packages.${system}.default;
        });

      apps = forEachSystem
        (system: {
          default = {
            type = "app";
            program = packages.${system}.default.outPath;
          };
          l2 = {
            type = "app";
            program = packages.${system}.l2.outPath;
          };
        });

      devShells = forEachSystem (system: {
        default = mkDevnet {
          system = system;
          chainweb-node = inputs.chainweb-node.packages.${system}.default;
        };
        l2 = mkDevnet {
          system = system;
          chainweb-node = inputs.chainweb-node-l2.packages.${system}.default;
        };
      });

      mkContainer = inputs@{ devnetRunner, system }:
        let pkgs = nixpkgs.legacyPackages.${system};
        in pkgs.dockerTools.buildImage {
          name = "devnet";
          fromImage = pkgs.dockerTools.pullImage {
            imageName = "ubuntu";
            imageDigest = "sha256:965fbcae990b0467ed5657caceaec165018ef44a4d2d46c7cdea80a9dff0d1ea";
            sha256 = "10wlr8rhiwxmz1hk95s9vhkrrjkzyvrv6nshg23j86rw08ckrqnz";
            finalImageTag = "22.04";
            finalImageName = "ubuntu";
          };

          copyToRoot = pkgs.runCommand "contents" {} ''
            mkdir -p $out/root/.devenv
          '';

          config = {
            WorkingDir = "/root";
            Cmd = [ devnetRunner.outPath ];
          };
        };

      mkDevnetRunner = { devnet, system }:
        let config = devnet.config;
            pkgs = nixpkgs.legacyPackages.${system};
        in pkgs.writeShellScript "start-processes" ''
          export $(${pkgs.toybox}/bin/xargs < ${config.procfileEnv})
          ${config.procfileScript}
        '';

      mkDevnet = args@{ chainweb-node, system }:
        let
          overlays = [(self: super: {
            chainweb-node = bundle args.chainweb-node;
            chainweb-mining-client = bundle inputs.chainweb-mining-client.packages.${system}.default;
          })];
          pkgs = import nixpkgs { inherit system overlays; };
          bundle = pkgs.callPackage inputs.nix-exe-bundle {};
        in devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [
            modules/chainweb-node.nix
            modules/chainweb-mining-client.nix
            ({config, ...}: {
              # https://devenv.sh/reference/options/

              services.nginx.enable = true;
              services.nginx.httpConfig = ''
                # Service API endpoints
                upstream service-api {
                    server localhost:1848;
                }

                # Mining endpoints
                upstream mining-api {
                    ip_hash; # for work and solve we need sticky connections
                    server localhost:1848;
                }

                # P2P endpoints
                upstream peer-api {
                    server localhost:1789;
                }

                server {
                    # server_name api.devnet.chainweb.com
                    listen 1337;

                    # access_log /var/log/nginx/chainweb-api-access.log;
                    # error_log /var/log/nginx/chainweb-api-error.log;

                    # Service API endpoints
                    location = /info {
                        proxy_pass http://service-api;
                    }
                    location = /health-check {
                        proxy_pass http://service-api;
                    }
                    location ~ ^/chainweb/0.0/[0-9a-zA-Z\-\_]+/chain/[0-9]+/pact/ {
                        proxy_pass http://service-api;
                    }
                    location ~ ^/chainweb/0.0/[0-9a-zA-Z\-\_]+/chain/[0-9]+/(header|hash|branch|payload) {
                        proxy_pass http://service-api;
                    }
                    location ~ /chainweb/0.0/[0-9a-zA-Z\-\_]+/cut {
                        proxy_pass http://service-api;
                    }

                    # Optional Service APIs
                    location ~ ^/chainweb/0.0/[0-9a-zA-Z\-\_]+/rosetta/ {
                        proxy_pass http://service-api;
                    }
                    location ~ /chainweb/0.0/[0-9a-zA-Z\-\_]+/header/updates {
                        proxy_buffering off;
                        proxy_pass http://service-api;
                    }

                    # Mining
                    location /chainweb/0.0/[0-9a-zA-Z\-\_]+/mining/ {
                        proxy_buffering off;
                        proxy_pass http://mining-api;
                    }

                    # Config (P2P API)
                    location = /config {
                        proxy_pass https://peer-api;
                        # needed if self signed certificates are used for nodes:
                        # proxy_ssl_verify off;
                    }

                    # Default
                    location / {
                        return 404;
                    }
                }
              '';
              process-managers.process-compose.enable = true;
              process.implementation = "process-compose";
              devenv.root = ".";
            })
          ];
        };
    };
}
