{ pkgs, lib, config, ...}:
with lib;
let
  strLen = builtins.stringLength;
  truncateMiddle = startLen: endLen: s:
    if strLen s <= startLen + endLen then s
    else strings.substring 0 startLen s + "..." + strings.substring (strLen s - endLen) endLen s;
  cfg = config.services.chainweb-data;
  port = toString cfg.port;
  absolutePgData = "$(${pkgs.coreutils}/bin/realpath ${config.env.PGDATA})";
  dbstring = "postgresql:///$USER?host=${absolutePgData}";
  chainweb-data-with-common-params = pkgs.writeShellScript "cwd-with-common-params" ''
    ${cfg.package}/bin/chainweb-data \
      --dbstring "${cfg.dbstring}" --service-host localhost \
      ${optionalString (cfg.migrations-folder != null)
        "--migrations-folder ${cfg.migrations-folder}"
      } \
      ${concatStringsSep " \\\n" (
        map
          (folder: "--extra-migrations-folder ${folder}")
          cfg.extra-migrations-folders
      )} \
      "$@"
  '' ;
  start-chainweb-data = pkgs.writeShellScript "start-chainweb-data" ''
    ${chainweb-data-with-common-params} --migrate \
      server --port ${port} --serve-swagger-ui
  '';
  psqlrc = pkgs.writeText ".psqlrc" ''
    \x auto
  '';
  psql-cwd = pkgs.writeShellScriptBin "psql-cwd" ''
    export PSQLRC="${psqlrc}"
    ${pkgs.postgresql}/bin/psql "${cfg.dbstring}"
  '';
  chainweb-data-fill = pkgs.writeShellScriptBin "chainweb-data-fill" ''
    ${chainweb-data-with-common-params} --ignore-schema-diff fill --level debug
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
    extra-migrations-folders = mkOption {
      type = types.listOf types.path;
      default = [];
      description = ''
        List of folders containing additional chainweb-data migrations.
      '';
    };
    throttle = mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Whether to throttle the chainweb-data endpoints. This is useful for
        public deployments.
      '';
    };
  };
  config = mkIf cfg.enable {
    packages = [
      cfg.package
      psql-cwd pkgs.less # psql needs less
      chainweb-data-fill
    ];

    processes.chainweb-data = {
      exec = "${pkgs.expect}/bin/unbuffer ${start-chainweb-data}";
      process-compose.depends_on = {
        chainweb-node.condition = "process_healthy";
        postgres.condition = "process_healthy";
      };
      process-compose.readiness_probe = {
        exec.command = ''
          until curl 'http://localhost:${port}/txs/events?limit=1&search=TRANSFER'; do sleep 0.1 ; done
        '';
        timeout_seconds = 1200;
      };
    };
    services.ttyd.commands.psql-cwd = "${psql-cwd}/bin/psql-cwd";
    services.ttyd.commands.chainweb-data-fill = "${chainweb-data-fill}/bin/chainweb-data-fill";

    services.postgres.enable= true;

    services.http-server = {
      upstreams.chainweb-data = "server localhost:${port};";
      extraHttpConfig = optionalString cfg.throttle ''
        limit_req_zone $binary_remote_addr zone=cwd:10m rate=10r/s;
      '';
      retry-after-duration = 1;
      servers.devnet.extraConfig = ''
        location ~ /(stats$|coins$|cwd-spec|txs|richlist.csv$) {
          proxy_pass http://chainweb-data;
          add_header Retry-After $retry_after always;

          ${optionalString cfg.throttle ''
            proxy_set_header Chainweb-Execution-Strategy Bounded;
            limit_req zone=cwd burst=2;
          ''}
        }
      '';
    };
    sites.landing-page.services.chainweb-data = {
      order = 10;
      markdown = ''
        ### Chainweb Data ${config.lib.packageVersionInfoMd cfg.package}

        Useful pages for interacting with the `chainweb-data` service.

        ${concatStringsSep "\n" (flatten [
          "- [Open API Spec](/cwd-spec/)"
          (optionals (config.services.ttyd.enable or false) [
            "- [DB Access](/ttyd/psql-cwd/)"
            "- [Run `fill` operation](/ttyd/chainweb-data-fill/)"
          ])
        ])}

        ${optionalString (builtins.length cfg.extra-migrations-folders > 0) ''
          The `chainweb-data` service is configured to use additional migrations
          from these folders:

          ${concatStringsSep "\n" (
            map (folder: "- `${truncateMiddle 20 40 folder}`") cfg.extra-migrations-folders
          )}
        ''}

        The `chainweb-data` service is configured to listen to port `${port}`,
        however, the public HTTP API is configured to proxy requests to this port.
      '';
    };
    sites.landing-page.commands.chainweb-data.markdown = ''
      * `psql-cwd`: Start a `psql` session as the `chainweb-data` service.
      * `chainweb-data-fill`: Run the `fill` operation of `chainweb-data`.
    '';
    sites.landing-page.container-api.ports =
      "- `${toString config.services.chainweb-data.port}`: Chainweb data API port";
    sites.landing-page.container-api.folders =
      "- `/cwd-extra-migrations`: `chainweb-data`'s extra migrations folder";
  };
}