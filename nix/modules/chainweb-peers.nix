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
    sleep 20s
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
      echo "chainweb-peers exited. Restarting in 10s..."
      sleep 60s
    done
  '';
  checkSqliteScript = pkgs.writeShellScript "check_sqlite.sh" ''
    #!/bin/bash

    DB_FILE="${databasePath}"
    RETRIES=100 Replace with the number of desired retries
    TOTAL_DURATION_MINS=60  # Replace with the total duration in minutes
    SLEEP_INTERVAL=$((TOTAL_DURATION_MINS * 60 / RETRIES))

    for ((i=1; i<=RETRIES; i++)); do
      # Check if the SQLite file exists
      if [ -f "$DB_FILE" ]; then
          # Check for the existence of specific tables and their state
          echo "Checking SQLite file $DB_FILE for tables and rows..."
          TABLE_CHECK=$(sqlite3 $DB_FILE "SELECT name FROM sqlite_master WHERE type='table' AND name='peers';")
          if [ -n "$TABLE_CHECK" ]; then
             # check if number of rows is greater than 0
             echo "Checking for rows in table peers..."
             ROW_COUNT=$(sqlite3 $DB_FILE "SELECT COUNT(*) FROM peers;")
             if [ "$ROW_COUNT" -gt 0 ]; then
                 echo "Checks passed."
                 exit 0
             fi
          fi
      fi

      # If not the last retry, sleep for the interval
      if [ $i -ne $RETRIES ]; then
          sleep $SLEEP_INTERVAL
      fi
    done

    echo "Checks failed after $RETRIES retries."
    exit 1
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
      process-compose.depends_on = {
        chainweb-node.condition = "process_healthy";
        # These two options below don't seem to work as expected
        # This will work so long as the sqlite file is not deleted between runs of the service
        # chainweb-peers.condition = "service_started";
        # script.condition = checkSqliteScript;
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

