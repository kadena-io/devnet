{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.ttyd;

  # Generate the bash script for ttyd to use.
  # It will check the `arg` URL parameter and use its value to execute the appropriate command.
  generatedScript = pkgs.writeShellScript "ttyd-wrapper.sh" ''
    case "$1" in
    ${concatStringsSep "\n"
      (attrsets.mapAttrsToList
        (name: command: ''
          "${name}")
            ${command}
            ;;
        '')
        cfg.commands
      )
    }
      *)
        echo "Command not recognized."
        ;;
    esac
  '';
in
{
  options.services.ttyd = {
    enable = mkEnableOption "ttyd web terminal service";

    commands = mkOption {
      default = {};
      type = types.attrsOf types.str;
      example = { myCommand = "echo hello"; };
      description = "Commands to be executed by ttyd based on the URL parameter.";
    };
  };

  config = mkIf cfg.enable {
    services.http-server.upstreams.ttyd = "server localhost:7861;";
    services.http-server.servers.devnet.extraConfig = ''
        location ~ ^/ttyd/([\w-]*)(.*)$ {
          proxy_http_version 1.1;
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header Connection "upgrade";
          proxy_pass http://127.0.0.1:7681/$2?arg=$1;
        }
      '';

    processes.ttyd.exec =
      "${pkgs.ttyd}/bin/ttyd --writable --interface 0.0.0.0 --url-arg --port 7681 ${generatedScript}";
  };
}
