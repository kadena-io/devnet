{ pkgs, lib, config, ...}:
with lib;
let
  cfg = config.services.chainweb-data;
  absolutePgData = "$(${pkgs.coreutils}/bin/realpath ${config.env.PGDATA})";
  dbstring = "postgresql:///$USER?host=${absolutePgData}";
  chainweb-data-with-conn-params = pkgs.writeShellScript "cwd-with-conn-params" ''
    ${cfg.package}/bin/chainweb-data \
      --dbstring "${cfg.dbstring}" --service-host localhost "$@"
  '' ;
  start-chainweb-data = pkgs.writeShellScript "start-chainweb-data" ''
    ${chainweb-data-with-conn-params} --migrate \
      ${optionalString (cfg.migrations-folder != null)
        "--migrations-folder ${cfg.migrations-folder}"
      } \
      ${optionalString (cfg.extra-migrations-folder != null)
        "--extra-migrations-folder ${cfg.extra-migrations-folder}"
      } \
      server --port ${toString cfg.port} --serve-swagger-ui
  '';
  psql-cwd = pkgs.writeShellScriptBin "psql-cwd" ''
    ${pkgs.postgresql}/bin/psql "${cfg.dbstring}"
  '';
  chainweb-data-fill = pkgs.writeShellScriptBin "chainweb-data-fill" ''
    ${chainweb-data-with-conn-params} fill --level debug
  '';
  links = flatten [
    "[Open API Spec](/cwd-spec/)"
    (optionals (config.services.ttyd.enable or false) [
      "[DB Access](/ttyd/psql-cwd/)"
      "[Run `fill` operation](/ttyd/chainweb-data-fill/)"
    ])
  ];
in
{
  options.services.chainweb-data = {
    enable = mkEnableOption "chainweb-data";
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.chainweb-data;
      defaultText = lib.literalExpression "pkgs.chainweb-data";
      description = "The chainweb-data package to use.";
    };
    port = mkOption {
      type = types.int;
      default = 1849;
      description = "The port to serve the chainweb-data service on.";
    };
    dbstring = mkOption {
      type = types.str;
      default = dbstring;
      description = "The database connection string for chainweb-data";
    };
    migrations-folder = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = ''
        The folder containing the chainweb-data migrations. If not set, the
        migrations shipped with the chainweb-data package will be used.
      '';
    };
    extra-migrations-folder = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = ''
        The folder containing additional chainweb-data migrations. If not set,
        no additional migrations will be used.
      '';
    };
  };
  config = mkIf cfg.enable {
    packages = [ cfg.package psql-cwd chainweb-data-fill ];

    processes.chainweb-data = {
      exec = "${pkgs.expect}/bin/unbuffer ${start-chainweb-data}";
      process-compose.depends_on = {
        chainweb-node.condition = "process_healthy";
        postgres.condition = "process_healthy";
      };
    };
    services.ttyd.commands.psql-cwd = "${psql-cwd}/bin/psql-cwd";
    services.ttyd.commands.chainweb-data-fill = "${chainweb-data-fill}/bin/chainweb-data-fill";

    processes.socat.exec = "${pkgs.socat}/bin/socat TCP-LISTEN:5432,reuseaddr,fork UNIX-CONNECT:${absolutePgData}/.s.PGSQL.5432";
    services.postgres.enable = true;
    services.http-server = {
      upstreams.chainweb-data = "server localhost:${toString cfg.port};";
      servers.devnet.extraConfig = ''
        location ~ /(stats$|coins$|cwd-spec|txs|richlist.csv$) {
          proxy_pass http://chainweb-data;
        }
      '';
    };
    sites.landing-page.services.chainweb-data = {
      order = 10;
      markdown = ''
        ### Chainweb Data

        Useful pages for interacting with the `chainweb-data` service.

        ${concatStringsSep "\n" (flatten [
          "- [Open API Spec](/cwd-spec/)"
          (optionals (config.services.ttyd.enable or false) [
            "- [DB Access](/ttyd/psql-cwd/)"
            "- [Run `fill` operation](/ttyd/chainweb-data-fill/)"
          ])
        ])}

        ${optionalString (cfg.extra-migrations-folder != null) ''
          The `chainweb-data` service is configured to use additional migrations
          from the folder `${cfg.extra-migrations-folder}`.
        ''}
      '';
    };
    sites.landing-page.commands.chainweb-data.markdown = ''
      * `psql-cwd`: Start a `psql` session as the `chainweb-data` service.
      * `chainweb-data-fill`: Run the `fill` operation of `chainweb-data`.
    '';
  };
}