{ pkgs, config, lib, ... }:

let
  cfg = config.services.chainweb-peers;
  start-chainweb-peers = pkgs.writeShellScript "start-chainweb-peers" ''
    ${pkgs.chainweb-peers}/bin/chainweb-peers \
    --config-file ${./chainweb-peers/chainweb-peers.yaml} \
    --bootstrap-node localhost:1789 \
    --peer-registry-connection '${peerRegistryConnection}' \
    --peers-file ${peersFile}
  '';
  databasePath = "${config.env.DEVENV_STATE}/chainweb-peers/peers.sqlite";
  peerRegistryConnection = builtins.toJSON {
    "sqlite-connection" = {
      "file-name" = databasePath;
    };
  };
  peersFile = "${config.env.DEVENV_STATE}/chainweb-peers/peers.json";
  elasticApiKey = config.chainweb-peers.elasticApiKey or "";
  elasticEndpoint = config.chainweb-peers.elasticEndpoint or "";
  csvFile = "${config.env.DEVENV_STATE}/chainweb-peers/tx-traces.csv";
  start-tx-traces = pkgs.writeShellScript "start-tx-traces" ''
    #!/bin/bash
    # RESPONSE=$(curl -sk -XPOST https://localhost:1789/chainweb/0.0/fast-development/chain/0/mempool/getPending)
    # echo $RESPONSE
    ${pkgs.tx-traces}/bin/tx-traces \
      --config-file ${./chainweb-peers/tx-traces.yaml} \
      --peer-registry-connection '${peerRegistryConnection}' \
      --csv-file ${csvFile}
  '';
  chainweb-peers-looper = pkgs.writeShellScript "chainweb-peers-looper.sh" ''
    #!/bin/bash
    mkdir -p ${config.env.DEVENV_STATE}/chainweb-peers
    while true; do
      echo "Starting chainweb-peers..."
      ${start-chainweb-peers}
      echo "chainweb-peers exited. Restarting in 30s..."
      sleep 30s
    done
  '';
in
{
  options.services.chainweb-peers = {
    enable = lib.mkEnableOption "chainweb-peers and tx-traces services";
  };
  config = lib.mkIf cfg.enable {
    packages = [ pkgs.chainweb-peers pkgs.tx-traces pkgs.sqlite ];

    processes.chainweb-peers = {
      exec = "${pkgs.expect}/bin/unbuffer ${chainweb-peers-looper}";
      process-compose.depends_on = {
        chainweb-node.condition = "process_healthy";
      };
    };
    processes.tx-traces = {
      exec = "${pkgs.expect}/bin/unbuffer ${start-tx-traces}";
      process-compose = {
        depends_on = {
          chainweb-node.condition = "process_healthy";
          chainweb-peers.condition = "service_started";
          txg.condition = "process_healthy";
        };
      };
    };

    # processes.chainweb-peers-dashboard = {
    #   exec = "${pkgs.echo}/bin/echo 'chainweb-peers-dashboard is not yet implemented'";
    # };

    services.ttyd.commands.chainweb-peers = "${start-chainweb-peers}/bin/start-chainweb-peers";

    services.elasticsearch.enable = true;

    sites.landing-page.services.chainweb-peers = {
      order = 10;
      markdown = ''
        ### Chainweb Peers

      '';
        # - [Chainweb Peers Dashboard](/chainweb-peers-dashboard)
    };
    sites.landing-page.commands.chainweb-peers.markdown = ''
      * `chainweb-peers-dashboard` - Chainweb Peers Dashboard
    '';
  };
}

