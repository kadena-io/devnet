{ pkgs, config, lib, ... }:

let
  cfg = config.services.txg;
  start-txg = pkgs.writeShellScript "start-txg" ''
    #!/bin/bash
    sleep 10s
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
      process-compose.depends_on = {
        chainweb-node.condition = "process_healthy";
        cut-checker.condition = "process_completed_successfully";
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

