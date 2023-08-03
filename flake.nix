{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
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
            , ... } @ inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let common-overlay = (self: super: {
            chainweb-data = bundle inputs.chainweb-data.packages.${system}.default;
            chainweb-mining-client = bundle inputs.chainweb-mining-client.packages.${system}.default;
          });
          l1-overlay = (self: super: {
            chainweb-node = bundle inputs.chainweb-node.packages.${system}.default;
          });
          l2-overlay = (self: super: {
            chainweb-node = bundle inputs.chainweb-node-l2.packages.${system}.default;
          });
          pkgs = nixpkgs.legacyPackages.${system};
          l1-pkgs = import nixpkgs { inherit system; overlays = [ common-overlay l1-overlay ]; };
          l2-pkgs = import nixpkgs { inherit system; overlays = [ common-overlay l2-overlay ]; };
          bundle = pkgs.callPackage inputs.nix-exe-bundle {};
      in rec {
        packages = rec {
          default = mkDevnetRunner { devnet = devShells.default; };
          l2 = mkDevnetRunner { devnet = devShells.l2; };
          container = mkContainer l1-pkgs;
          container-l2 = mkContainer l2-pkgs;
          cwd = inputs.chainweb-data.packages.default;
        };

        apps = {
          default = {
            type = "app";
            program = packages.default.outPath;
          };
          l2 = {
            type = "app";
            program = packages.l2.outPath;
          };
        };

        devShells = {
          default = mkDevnet { pkgs = l1-pkgs; };
          l2 = mkDevnet { pkgs = l2-pkgs; };
        };

        mkContainer = pkgs:
          let
            devnet = mkDevnet { inherit pkgs; };
            config = devnet.config;
            devnetRunner = mkDevnetRunner {inherit devnet;};
            baseImage = pkgs.dockerTools.buildImage {
              name = "devnet-base";
              copyToRoot = pkgs.buildEnv {
                name = "devnet-base-root";
                paths = [
                  config.services.postgres.package
                  config.services.nginx.package
                  pkgs.chainweb-node
                  pkgs.chainweb-mining-client
                  pkgs.coreutils
                  pkgs.findutils
                  pkgs.bashInteractive
                  pkgs.su
                  pkgs.nano
                ];
              };
            };
          in pkgs.dockerTools.buildImage {
            name = "devnet";
            fromImage = baseImage;

            runAsRoot = ''
              #!${pkgs.runtimeShell}
              ${pkgs.dockerTools.shadowSetup}

              mkdir /tmp
              chmod 777 /tmp

              # Nginx needs a nobody:nogroup
              groupadd -r nogroup
              useradd -r -g nogroup nobody

              groupadd -r devnet
              useradd -r -g devnet -d /devnet devnet
              mkdir -p /devnet/.devenv
              chown -R devnet:devnet /devnet
            '';

            config = {
              WorkingDir = "/devnet";
              Cmd = [ devnetRunner.outPath ];
              User = "devnet";
            };
          };

        mkDevnetRunner = { devnet }:
          let config = devnet.config;
              pkgs = nixpkgs.legacyPackages.${system};
          in pkgs.writeShellScript "start-processes" ''
            export $(${pkgs.findutils}/bin/xargs < ${config.procfileEnv})
            ${config.procfileScript}
          '';

        mkDevnet = args@{ pkgs }: devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [
            modules/chainweb-data.nix
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
              process.implementation = "process-compose";
              devenv.root = ".";
            })
          ];
        };
      }
    );
}
