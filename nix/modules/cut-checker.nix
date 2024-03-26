{ pkgs, config, lib, ... }:

let
  cfg = config.services.cut-checker;
  cut-checker = pkgs.writeShellScript "cut-checker" ''
    #!/bin/bash
    echo "Checking for cut"
    for (( i=1; i <= 100; i++ ))
    do
        RESPONSE=$(curl -sk -XGET https://localhost:1789/chainweb/0.0/fast-development/cut)
        echo $RESPONSE | jq empty 2> /dev/null

        if [[ $? -eq 0 ]]; then
            echo "Cut found"
            exit 0
        fi
    done
    echo "Cut found"
    exit 1
    '';
in {
   options.services.cut-checker = {
     enable = lib.mkEnableOption "cut-checker";
   };
   config = lib.mkIf cfg.enable {
     packages = [ pkgs.curl pkgs.jq ];
     processes.cut-checker = {
       exec = "${pkgs.expect}/bin/unbuffer ${cut-checker}";
       process-compose.depends_on = {
         chainweb-node.condition = "process_healthy";
       };
     };
   };
}
