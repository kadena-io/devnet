{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.http-server;
in
{
  options.services.http-server = {
    enable = mkEnableOption "HTTP server";
    nginx-modules = mkOption {
      type = types.listOf types.anything;
      description = "The Nginx modules to enable.";
    };
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
    retry-after-duration = mkOption {
      type = types.nullOr types.int;
      default = null;
      description = "Duration set to $retry-after when response status is 429.";
    };
    extraHttpConfig = mkOption {
      type = types.lines;
      description = "Additional HTTP configurations.";
      default = "";
    };
  };

  config = mkIf cfg.enable {
    services.nginx.enable = true;
    services.nginx.package = pkgs.nginx.override {
      modules = cfg.nginx-modules;
    };

    services.http-server.nginx-modules = pkgs.nginx.modules;

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

      mapRetryAfter = optionalString (cfg.retry-after-duration != null) ''
        map $status $retry_after {
            default ''';
            429 '${toString cfg.retry-after-duration}';
        }
      '';

      mapConnectionUpgrade = ''
        # $connection_upgrade is used for websocket proxying
        map $http_upgrade $connection_upgrade {
            default upgrade;
            '''      close;
        }
      '';

      in indentString ''
        ${mapConnectionUpgrade}
        ${mapRetryAfter}

        ${indentString cfg.extraHttpConfig}

        ${upstreamConfig}

        ${serverConfig}
      '';
    sites.landing-page.container-api.ports = mkBefore "- `8080`: Public HTTP API";
  };
}