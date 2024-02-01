{pkgs, config, lib, ...}:

let
  cfg = config.services.show-es-output;
  show-es-output = pkgs.writeShellScript "show-es-output" ''
  #!/bin/bash

   # Elasticsearch server details
   ES_HOST=${cfg.host}
   ES_PORT=${toString cfg.port}
   INDEX_NAME=${cfg.index-name}

   # Elasticsearch query
   QUERY='{
     "query": {
       "match_all": {}
     }
   }'

   # Send the search request
   while true; do
     curl -X GET "http://$ES_HOST:$ES_PORT/$INDEX_NAME/_search" -H 'Content-Type: application/json' -d "$QUERY" | jq .
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
     index-name = lib.mkOption {
       type = lib.types.str;
       default = "chainweb-fast-development-txg";
     };
     sleep-interval = lib.mkOption {
       type = lib.types.int;
       default = 10;
     };
   };
   config = lib.mkIf cfg.enable {
     packages = [ pkgs.curl pkgs.jq ];
     processes.show-es-output = {
       exec = "${pkgs.expect}/bin/unbuffer ${show-es-output}";
       process-compose.depends_on = {
         elasticsearch.condition = "process_healthy";
         txg.condition = "process_healthy";
       };
     };
   };
}
