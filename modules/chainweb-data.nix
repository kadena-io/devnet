{ pkgs, lib, config, ...}:
with lib;
let
  cfg = config.services.chainweb-data;
  absolutePgData = "$(${pkgs.coreutils}/bin/realpath ${config.env.PGDATA})";
  dbstring = "postgresql:///$USER?host=${absolutePgData}";
  chainweb-data-with-conn-params = pkgs.writeShellScript "cwd-with-conn-params" ''
    ${pkgs.chainweb-data}/bin/chainweb-data \
      --dbstring "${cfg.dbstring}" --service-host localhost "$@"
  '' ;
  start-chainweb-data = pkgs.writeShellScript "start-chainweb-data" ''
    ${chainweb-data-with-conn-params} --migrate server --port 1849 --serve-swagger-ui
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
    dbstring = mkOption {
      type = types.str;
      default = dbstring;
      description = "The database connection string for chainweb-data";
    };
  };
  config = mkIf cfg.enable {
    packages = [ pkgs.chainweb-data psql-cwd chainweb-data-fill ];

    processes.chainweb-data = {
      exec = "${pkgs.expect}/bin/unbuffer ${start-chainweb-data}";
      process-compose.depends_on = {
        chainweb-node.condition = "process_healthy";
        postgres.condition = "process_healthy";
      };
    };
    services.ttyd.commands.psql-cwd = "${psql-cwd}/bin/psql-cwd";
    services.ttyd.commands.chainweb-data-fill = "${chainweb-data-fill}/bin/chainweb-data-fill";

    services.postgres.enable = true;
    services.http-server = {
      upstreams.chainweb-data = "server localhost:1849;";
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

        ${concatStringsSep "\n" (flatten [
          "- [Open API Spec](/cwd-spec/)"
          (optionals (config.services.ttyd.enable or false) [
            "- [DB Access](/ttyd/psql-cwd/)"
            "- [Run `fill` operation](/ttyd/chainweb-data-fill/)"
          ])
        ])}
      '';
    };
    sites.landing-page.commands.chainweb-data.markdown = ''
      * `psql-cwd`: Start a `psql` session as the `chainweb-data` service.
      * `chainweb-data-fill`: Run the `fill` operation of `chainweb-data`.
    '';
  };
}