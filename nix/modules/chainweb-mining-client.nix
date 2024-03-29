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
  envVars = {
    MINING_IDLE_PERIOD = {
      description = ''
        The average time, in seconds, it takes to mine blocks and advance the block height
        by one while the network is idle (i.e. no incoming transactions)
      '';
      summary = "Time to mint when idle";
      default = 30.0;
    };
    MINING_DISABLE_IDLE = {
      description = ''
        Disable periodic mining when the network is idle. Note that this is *NOT RECOMMENDED*
        for most cases, since in the absence of mining, the node's current time will
        lag behind and transactions will not be accepted. Consider increasing MINING_IDLE_PERIOD
        instead.
      '';
      summary = "Disable idle mining";
      default = false;
      type = types.bool;
    };
    MINING_BATCH_PERIOD = {
      description = ''
        Wait for this period, in seconds, after receiving a transaction and then mine blocks
        on the chain where the transaction was received. This period is used to batch transactions
        and avoid mining a block for each transaction. Increasing this period also makes mining
        more realistic compared to the public networks
      '';
      summary = "Waiting period after transaction";
      default = 5.0;
    };
    MINING_CONFIRMATION_COUNT = {
      description = ''
        The number of blocks to mine faster after transactions; This makes it quicker to get
        a transaction confirmed.
      '';
      summary = "Number of confirmation blocks";
      default = 5;
      type = types.int;
    };
    MINING_CONFIRMATION_PERIOD = {
      description = ''
        The period, in seconds, to wait after minting the confirmation blocks of transactions
      '';
      summary = "Confirmation period";
      default = 5.0;
    };
    MINING_DISABLE_CONFIRMATION = {
      description = ''
        Disable quick mining for confirming transactions. Note that if you want to mint
        the blocks containing transactions only and no further confirmations, don't disable
        this option and use MINING_CONFIRMATION_COUNT=1 instead.
      '';
      summary = "Disable confirmation mining";
      default = false;
      type = types.bool;
    };
  };
  envValue = name: builtins.toString (builtins.toJSON cfg.envVars.${name});
  spliceVariable = name: "\"\${${name}:-${envValue name}}\"";
in
{
  options.services.chainweb-mining-client = with types; {
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
    envVars = mapAttrs (_name: details: mkOption {
      type = details.type or float;
      inherit (details) default description;
    }) envVars;
    expose-make-blocks = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Expose the `/make-blocks` endpoint on the HTTP server. This endpoint is used to trigger
        mining explicitly. This option is useful for testing and development purposes, but
        it is not recommended for publicly accessible deployments.
      '';
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
          --idle-trigger-period ${spliceVariable "MINING_IDLE_PERIOD"} \
          --confirmation-trigger-period ${spliceVariable "MINING_CONFIRMATION_PERIOD"} \
          --transaction-batch-period ${spliceVariable "MINING_BATCH_PERIOD"} \
          --confirmation-count ${spliceVariable "MINING_CONFIRMATION_COUNT"} \
          --disable-idle-worker ${spliceVariable "MINING_DISABLE_IDLE"} \
          --disable-confirmation-worker ${spliceVariable "MINING_DISABLE_CONFIRMATION"} \
      '').outPath;
      process-compose.depends_on = {
        chainweb-node.condition = "process_healthy";
        chainweb-mining-client.condition = "process_healthy";
      };
    };
    services.http-server = {
      upstreams.chainweb-mining-client = "server localhost:${on-demand-port};";
      servers.devnet.extraConfig = mkMerge [
        (mkIf cfg.expose-make-blocks ''
          location = /make-blocks {
            proxy_pass http://chainweb-mining-client/make-blocks;
            proxy_buffering off;
          }
        '')
        (mkBefore ''
          location ~ ^/chainweb/0.0/[0-9a-zA-Z\-\_]+/chain/[0-9]+/pact/api/v1/send {
            proxy_pass http://localhost:1791;
          }
        '')
      ];
    };
    sites.landing-page = {
      services.chainweb-mining-client = {
        order = 8;
        markdown = ''
          ### Flexible Mining
          This container comes with a `chainweb-mining-client` in `on-demand` mode and a
          `mining-trigger` service that triggers mining periodically and also in response
          to incoming transactions. You can customise the mining behaviour by setting
          [environment variables](#mining-env-vars). ${optionalString cfg.expose-make-blocks ''
            You can also trigger mining explicitly
            by sending [`POST /make-blocks`](https://github.com/kadena-io/chainweb-mining-client/tree/master?tab=readme-ov-file#non-pow-mining)
            requests.
          ''}

          <details> <summary id="mining-env-vars" > **Environment Variables** </summary>

          <div style="margin-left:10px">
          ${concatMapStringsSep "\n" (name: with envVars.${name}; ''
            #### ${name}
            ${description} (default: ${envValue name})
          '') (builtins.attrNames envVars)}
          </div>

          </details>
        '';
      };
      container-api = {
        ports = lib.concatStringsSep "\n" [
          "- `${on-demand-port}`: On-Demand Mining API"
          "- `${mining-trigger-port}`: Mining Trigger API"
        ];
        envVars = concatMapStringsSep "\n" (name: with envVars.${name};
          "- [`${name}`](#${toLower name}): ${summary} (default: ${envValue name})"
        ) (builtins.attrNames envVars);
      };
    };
  };
}
