{ pkgs, lib, config, ...}:
with lib;
let
  cfg = config.services.chainweb-mining-client;
  on-demand-port = toString cfg.on-demand-port;
  cwn-service-port = toString config.services.chainweb-node.service-port;
  mining-trigger-port = toString cfg.mining-trigger-port;
  start-chainweb-mining-client = pkgs.writeShellScript "start-chainweb-mining-client" ''
    ${pkgs.chainweb-mining-client}/bin/chainweb-mining-client \
    --public-key=f89ef46927f506c70b6a58fd322450a936311dc6ac91f4ec3d8ef949608dbf1f \
    --node=127.0.0.1:1848 \
    --worker=on-demand \
    --on-demand-port=${on-demand-port} \
    --thread-count=1 \
    --log-level=info \
    --no-tls
  '';
in
{
  options.services.chainweb-mining-client = {
    enable = mkEnableOption "Enable the chainweb-mining-client service";
    on-demand-port = mkOption {
      type = types.int;
      default = 1790;
      description = "The port to listen on for on-demand mining requests.";
    };
    mining-trigger-port = mkOption {
      type = types.nullOr types.int;
      default = 1791;
      description = "The mining-trigger port";
    };
  };
  config = mkIf cfg.enable {
    packages = [ pkgs.chainweb-mining-client ];
    processes.chainweb-mining-client = {
      exec = "${start-chainweb-mining-client}";
      process-compose = {
        depends_on.chainweb-node.condition = "process_healthy";
        readiness_probe.exec.command = ''
          until curl http://localhost:${on-demand-port}/make-blocks -d '{}'
            do sleep 0.1; done
        '';
      };
    };
    processes.mining-trigger = {
      exec = (pkgs.writeShellScript "start-mining-trigger" ''
        ${pkgs.expect}/bin/unbuffer ${pkgs.mining-trigger}/bin/mining-trigger \
          --port ${mining-trigger-port} \
          --mining-client-url http://localhost:${on-demand-port} \
          --chainweb-service-endpoint http://localhost:${cwn-service-port} \
          --idle-trigger-period "''${MINING_IDLE_PERIOD:-30}" \
          --confirmation-trigger-period "''${MINING_CONFIRMATION_PERIOD:-5}" \
          --transaction-batch-period "''${MINING_TRANSACTION_BATCH_PERIOD:-5}" \
          --default-confirmation-count "''${MINING_DEFAULT_CONFIRMATION_COUNT:-5}" \
      '').outPath;
      process-compose.depends_on = {
        chainweb-node.condition = "process_healthy";
        chainweb-mining-client.condition = "process_healthy";
      };
    };
    services.http-server = {
      upstreams.chainweb-mining-client = "server localhost:${on-demand-port};";
      servers.devnet.extraConfig = mkBefore ''
        location = /make-blocks {
          proxy_pass http://chainweb-mining-client/make-blocks;
          proxy_buffering off;
        }
        location ~ ^/chainweb/0.0/[0-9a-zA-Z\-\_]+/chain/[0-9]+/pact/api/v1/send {
          proxy_pass http://localhost:1791;
        }
      '';
    };
    sites.landing-page.services.chainweb-mining-client = {
      order = 8;
      markdown = ''
        ### On-Demand Mining
        This container has a chainweb-mining-client in on-demand mode.
        You can produce blocks by sending a POST request to the `/make-blocks` endpoint.
        (see [the relevant section in the chainweb-mining-client README](https://github.com/kadena-io/chainweb-mining-client/tree/master?tab=readme-ov-file#non-pow-mining))
      '';
    };
    sites.landing-page.container-api.ports = lib.concatStringsSep "\n" [
      "- `${on-demand-port}`: On-Demand Mining API"
      "- `${mining-trigger-port}`: Mining Trigger API"
    ];
  };
}
