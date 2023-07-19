{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    devenv.url = "github:kadena-io/devenv/devnet-setup";
    chainweb-node.url = "github:kadena-io/chainweb-node/edmund/fast-devnet";
    chainweb-data.url = "github:kadena-io/chainweb-data";
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
        (system: {
          default =
            let config = devShells.${system}.default.config;
                pkgs = nixpkgs.legacyPackages.${system};
            in pkgs.writeShellScript "start-processes" ''
              ${pkgs.python3Packages.python-dotenv}/bin/dotenv \
                -f ${config.procfileEnv} run ${config.procfileScript}
            '';
        });

      apps = forEachSystem
        (system: {
          default = {
            type = "app";
            program = packages.${system}.default.outPath;
          };
        });


      devShells = forEachSystem
        (system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
            bundle = pkgs.callPackage inputs.nix-exe-bundle {};
            bundleCond = drv: if system == "aarch64-darwin" then bundle drv else drv;
            chainweb-node = bundleCond inputs.chainweb-node.packages.${system}.default;
            chainweb-mining-client = bundleCond inputs.chainweb-mining-client.packages.${system}.default;
            start-chainweb-node = stateDir: pkgs.writeShellScript "start-chainweb-node" ''
              ${chainweb-node}/bin/chainweb-node \
              --config-file=${./chainweb/config/chainweb-node.common.yaml} \
              --p2p-certificate-chain-file=${./chainweb/devnet-bootstrap-node.cert.pem} \
              --p2p-certificate-key-file=${./chainweb/devnet-bootstrap-node.key.pem} \
              --p2p-hostname=bootstrap-node \
              --bootstrap-reachability=2 \
              --cluster-id=devnet-minimal \
              --p2p-max-session-count=3 \
              --mempool-p2p-max-session-count=3 \
              --known-peer-info=YNo8pXthYQ9RQKv1bbpQf2R5LcLYA3ppx2BL2Hf8fIM@bootstrap-node:1789 \
              --log-level=info \
              --enable-mining-coordination \
              --mining-public-key=f90ef46927f506c70b6a58fd322450a936311dc6ac91f4ec3d8ef949608dbf1f \
              --header-stream \
              --rosetta \
              --allowReadsInLocal \
              --database-directory=${stateDir}/chainweb/db \
              --disable-pow
            '';
            start-chainweb-mining-client = pkgs.writeShellScript "start-chainweb-mining-client" ''
              ${chainweb-mining-client}/bin/chainweb-mining-client \
              --public-key=f90ef46927f506c70b6a58fd322450a936311dc6ac91f4ec3d8ef949608dbf1f \
              --node=127.0.0.1:1848 \
              --worker=constant-delay \
              --constant-delay-block-time=5 \
              --thread-count=1 \
              --log-level=info \
              --no-tls
            '';
          in
          {
            default = devenv.lib.mkShell {
              inherit inputs pkgs;
              modules = [
                ({config, ...}: {
                  # https://devenv.sh/reference/options/
                  packages = [
                    chainweb-node
                    chainweb-mining-client
                    pkgs.nodejs-18_x
                  ];

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
                        location = /chainweb/0.0/[0-9a-zA-Z\-\_]+/cut {
                            proxy_pass http://service-api;
                        }

                        # Optional Service APIs
                        location ~ ^/chainweb/0.0/[0-9a-zA-Z\-\_]+/rosetta/ {
                            proxy_pass http://service-api;
                        }
                        location = /chainweb/0.0/[0-9a-zA-Z\-\_]+/header/updates {
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
                  processes.chainweb-node = {
                    exec = "${start-chainweb-node config.env.DEVENV_STATE}";
                    process-compose.readiness_probe = {
                      http_get = {
                        host = "127.0.0.1";
                        scheme = "http";
                        port = 1848;
                        path = "/health-check";
                      };
                      initial_delay_seconds = 5;
                      period_seconds = 10;
                      timeout_seconds = 30;
                      success_threshold = 1;
                      failure_threshold = 10;
                    };
                  };
                  processes.chainweb-mining-client = {
                    exec = "${start-chainweb-mining-client}";
                    process-compose = {
                      depends_on.chainweb-node.condition = "process_healthy";
                    };
                  };
                  devenv.root = ".";

                  # Work around a process-compose log display bug, remove these lines
                  # once we start using a process-compsoe with the following merged:
                  # https://github.com/F1bonacc1/process-compose/pull/74 is merged
                  processes.chainweb-node.process-compose.disable_ansi_colors = true;
                  processes.nginx.process-compose.disable_ansi_colors = true;
                  processes.chainweb-mining-client.process-compose.disable_ansi_colors = true;
                })
              ];
            };
          });
    };
}
