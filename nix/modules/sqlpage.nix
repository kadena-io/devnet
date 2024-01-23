{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.sqlpage;
  workingDirectory = pkgs.runCommand "sqlpage-working-directory" {} ''
    mkdir -p $out

    # Assemble web_root from the options.services.sqlpage.pages configuration
    # note that the attribute names can have slashes in them, so we need to
    # create the directories as we go.
    cd $out
    ${
      lib.concatMapStringsSep "\n" (page: ''
        dir=$(dirname ${page})
        name=$(basename ${page})
        mkdir -p $dir
        ln -s ${cfg.pages.${page}} $dir/$name.sql
      '') (lib.attrNames cfg.pages)
    }

    mkdir -p $out/sqlpage/templates
    cd $out/sqlpage/templates
    ${
      lib.concatMapStringsSep "\n" (template: ''
        ln -s ${cfg.templates.${template}} ${template}.handlebars
      '') (lib.attrNames cfg.templates)
    }
  '';
  port = toString cfg.port;
in {
  options.services.sqlpage = with types; {
    enable = lib.mkEnableOption "SQLPage";

    pages = mkOption {
      type = types.attrsOf types.path;
      default = {};
      description = "Attribute set of paths to SQL files to be used by SQLPage.";
    };

    templates = mkOption {
      type = types.attrsOf types.path;
      default = {};
      description = "Attribute set of paths to template files to be used by SQLPage.";
    };

    port = mkOption {
      type = types.int;
      default = 8090;
      description = "Port to listen on.";
    };
  };

  config = mkIf cfg.enable {
    services.http-server = {
      upstreams.sqlpage = "server localhost:${port};";
      servers.devnet.extraConfig = ''
          location /sqlpage/ {
              proxy_pass http://sqlpage/;
          }
          location /sqlpage. {
              proxy_pass http://sqlpage;
          }
          location /apexcharts. {
              proxy_pass http://sqlpage;
          }
          location /tabler-icons. {
              proxy_pass http://sqlpage;
          }
      '';
    };

    # Available at <http-api>/sqlpage/example.sql
    services.sqlpage.pages."example" = pkgs.writeText "example.sql" ''
      SELECT 'hero' AS component
           , 'Example SQLPAGE' AS title
           , 'This page serves as an example for implementing other sqlpages' AS description
      ;
      SELECT 'datagrid' AS component
           , 'Devnet Overview' AS title
           , 'telescope' AS icon -- From https://tabler.io/icons
      ;
      SELECT 'Devnet Height' AS title
           , 'stack-3' AS icon -- From https://tabler.io/icons
           , (SELECT max(height) FROM blocks) AS description
      ;
    '';

    processes.sqlpage = {
      exec = "DATABASE_URL=postgresql:///$USER?host=${config.env.PGDATA} ${pkgs.sqlpage}/bin/sqlpage";
      process-compose = {
        working_dir = workingDirectory;
        environment = [
          "PORT=${port}"
        ];
      };
    };
  };
}
