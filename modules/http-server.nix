{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.http-server;
in
{
  options.services.http-server = {
    enable = mkEnableOption "HTTP server";
    upstreams = mkOption {
      default = {};
      type = types.attrsOf types.str;
      description = "Upstream configurations for the HTTP server.";
      example = { someapi = "server localhost:1848"; };
    };

    servers = mkOption {
      default = {};
      type = types.attrsOf (types.submodule {
        options = {
          listen = mkOption {
            type = types.int;
            default = 8080;
            description = "Port to listen on.";
          };
          extraConfig = mkOption {
            type = types.lines;
            default = "";
            description = "Additional server configurations, e.g., location blocks.";
            example = ''
              location /info {
                proxy_pass http://someapi;
              }
              location /static {
                root /var/www/static;
              }
            '';
          };
        };
      });
      description = "Server blocks configurations.";
    };
  };

  config = mkIf cfg.enable {
    services.nginx.enable = true;
    services.nginx.httpConfig = let
      indentString = s: builtins.replaceStrings ["\n"] ["\n  "] s;
      upstreamConfig = concatStringsSep "\n" (mapAttrsToList (name: value: ''
        upstream ${name} {
          ${indentString value}
        }
      '') cfg.upstreams);

      serverConfig = concatStringsSep "\n" (mapAttrsToList (name: server: ''
        server {
          listen ${toString server.listen};
          ${indentString server.extraConfig}
        }
      '') cfg.servers);

    in indentString ''
      ${upstreamConfig}

      ${serverConfig}
    '';
  };
}