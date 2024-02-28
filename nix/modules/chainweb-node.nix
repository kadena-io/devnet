{ pkgs, lib, config, ... }:
let
  cfg = config.services.chainweb-node;
  service-port = toString cfg.service-port;
  start-chainweb-node = stateDir: pkgs.writeShellScript "start-chainweb-node" ''
    ${cfg.package}/bin/chainweb-node \
    --config-file=${./chainweb/chainweb-node.common.yaml} \
    --p2p-certificate-chain-file=${./chainweb/devnet-bootstrap-node.cert.pem} \
    --p2p-certificate-key-file=${./chainweb/devnet-bootstrap-node.key.pem} \
    --p2p-hostname=bootstrap-node \
    --bootstrap-reachability=2 \
    --cluster-id=devnet-minimal \
    --p2p-max-session-count=3 \
    --mempool-p2p-max-session-count=3 \
    --p2p-port=${toString cfg.p2p-port} \
    --known-peer-info=YNo8pXthYQ9RQKv1bbpQf2R5LcLYA3ppx2BL2Hf8fIM@bootstrap-node:1789 \
    --log-level=info \
    --enable-mining-coordination \
    --mining-public-key=f89ef46927f506c70b6a58fd322450a936311dc6ac91f4ec3d8ef949608dbf1f \
    --header-stream \
    --rosetta \
    --allowReadsInLocal \
    --database-directory=${stateDir}/chainweb/db \
    --disable-pow \
    --service-port=${toString cfg.service-port}
  '';
  throttleDirectives = lib.optionalString cfg.throttle ''
    limit_req zone=cwn burst=200;
    add_header Retry-After $retry_after always;
  '';
in
{
  options.services.chainweb-node = {
    enable = lib.mkEnableOption "Enable the chainweb-node service.";
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.chainweb-node;
      defaultText = lib.literalExpression "pkgs.chainweb-node";
      description = "The chainweb-node package to use.";
    };
    service-port = lib.mkOption {
      type = lib.types.port;
      default = 1848;
      description = "The port on which the chainweb-node service endpoint listens.";
    };
    p2p-port = lib.mkOption {
      type = lib.types.port;
      default = 1789;
      description = "The port on which the chainweb-node p2p endpoint listens.";
    };
    throttle = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Whether to throttle the chainweb-node endpoints. This is useful for
        public deployments.
      '';
    };
  };
  config = lib.mkIf cfg.enable {
    packages = [ cfg.package ];
    processes.chainweb-node = {
      exec = "${start-chainweb-node config.env.DEVENV_STATE}";
      process-compose.readiness_probe = {
        exec.command = ''
          until curl http://localhost:${service-port}/health-check; do sleep 0.1 ; done
        '';
        timeout_seconds = 10;
      };
    };

    sites.landing-page.services.chainweb-node = {
      order = 0;
      markdown = ''
        ### Chainweb Node ${config.lib.packageVersionInfoMd cfg.package}

        The Chainweb Node service is running on this node. Its service endpoint is
        available at port `${toString cfg.service-port}`, however the public HTTP API
        is configured to proxy requests to this port.
      '';
    };
    sites.landing-page.container-api.ports = lib.concatStringsSep "\n" [
      "- `${toString cfg.service-port}`: Chainweb node's service port"
      "- `${toString cfg.p2p-port}`: Chainweb node's p2p API port"
    ];
    sites.landing-page.commands.chainweb-node.markdown = ''
      * `cwtool`: A collection of tools that are helpful for maintaining, testing, and debugging Chainweb
    '';

    services.http-server = {
      upstreams = {
        service-api = "server localhost:${toString cfg.service-port};";
        mining-api = ''
          ip_hash; # for work and solve we need sticky connections
          server localhost:${toString cfg.service-port};
        '';
        peer-api = "server localhost:1789;";
      };
      extraHttpConfig = lib.optionalString cfg.throttle ''
        limit_req_zone $binary_remote_addr zone=cwn:10m rate=20r/s;
        limit_req_status 429;
        limit_conn_status 429;
        map $status $retry_after {
            default ''';
            429 '1';
        }
      '';
      retry-after-duration = 1;
      servers.devnet = {
        extraConfig = ''
          location = /info {
            proxy_pass http://service-api;
            ${throttleDirectives}
          }
          location = /health-check {
            proxy_pass http://service-api;
          }
          location ~ ^/chainweb/0.0/[0-9a-zA-Z\-\_]+/chain/[0-9]+/pact/ {
            proxy_pass http://service-api;
            ${throttleDirectives}
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
            ${throttleDirectives}
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
