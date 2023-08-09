{ pkgs, config, ... }:
let
  chainweb-node = pkgs.chainweb-node;
  start-chainweb-node = stateDir: pkgs.writeShellScript "start-chainweb-node" ''
    ${chainweb-node}/bin/chainweb-node \
    --config-file=${./chainweb/chainweb-node.common.yaml} \
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
in
{
  config = {
    packages = [ chainweb-node ];
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

    services.http-server = {
      upstreams = {
        service-api = "server localhost:1848;";
        mining-api = ''
          ip_hash; # for work and solve we need sticky connections
          server localhost:1848;
        '';
        peer-api = "server localhost:1789;";
      };
      servers.devnet = {
        extraConfig = ''
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
        '';
      };
    };
  };
}