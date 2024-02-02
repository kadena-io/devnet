{pkgs, config, lib, ...}:

let
  cfg = config.services.show-es-output;
  show-es-output = pkgs.writeShellScript "show-es-output" ''

   # Elasticsearch server details
   ES_HOST=${cfg.host}
   ES_PORT=${toString cfg.port}
   TX_SUBMISSION_INDEX=${cfg.tx-submission-index}
   TX_TRACE_INDEX=${cfg.tx-trace-index}

   # Send the search requests
   while true; do
     ${pkgs.curl}/bin/curl -H 'Content-Type: application/json' -X GET "http://$ES_HOST:$ES_PORT/$TX_SUBMISSION_INDEX/_search?pretty"
     ${pkgs.curl}/bin/curl -H 'Content-Type: application/json' -X GET "http://$ES_HOST:$ES_PORT/$TX_TRACE_INDEX/_search?pretty"
     sleep ${toString cfg.sleep-interval}
   done

  '';
in {
   options.services.show-es-output = {
     enable = lib.mkEnableOption "show-es-output";
     port = lib.mkOption {
       type = lib.types.port;
       default = 9200;
     };
     host = lib.mkOption {
       type = lib.types.str;
       default = "localhost";
     };
     apiKey = lib.mkOption {
       type = lib.types.str;
       default = "";
     };
     tx-submission-index = lib.mkOption {
       type = lib.types.str;
       default = "chainweb-fast-development-txg";
     };
     tx-trace-index = lib.mkOption {
       type = lib.types.str;
       default = "fast-development-nodes";
     };
     sleep-interval = lib.mkOption {
       type = lib.types.int;
       default = 60;
     };
   };
   config = lib.mkIf cfg.enable {
     packages = [ pkgs.curl pkgs.jq ];
     processes.show-es-output = {
       exec = "${pkgs.expect}/bin/unbuffer ${show-es-output}";
       process-compose.depends_on = {
         elasticsearch.condition = "process_healthy";
         txg.condition = "service_started";
       };
     };
   };
}
