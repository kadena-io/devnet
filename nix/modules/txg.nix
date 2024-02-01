{ pkgs, config, lib, ... }:

let
  cfg = config.services.txg;
  start-txg = pkgs.writeShellScript "start-txg" ''
    #!/bin/bash
    ${pkgs.txg}/bin/txg --config-file ${./txg/run-simple-expressions.yaml}
  '';
in {
  options.services.txg = {
    enable = lib.mkEnableOption "txg";
  };
  config = lib.mkIf cfg.enable {
    packages = [ pkgs.txg ];
    processes.txg = {
      exec = "${pkgs.expect}/bin/unbuffer ${start-txg}";
      process-compose = {
        depends_on = {
         chainweb-node.condition = "process_healthy";
         cut-checker.condition = "process_completed_successfully";
         elasticsearch.condition = "process_healthy";
        };
        readiness_probe = {
          exec.command = "${pkgs.curl}/bin/curl -f -k http://localhost:9200/_cluster/health?pretty";
          initial_delay_seconds = 40;
          period_seconds = 5;
          timeout_seconds = 30;
        };
      };
    };
    sites.landing-page.services.txg = {
      order = 10;
      markdown = ''
        ### Transaction Generator

      '';
    };
  };
}

