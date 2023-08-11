{ pkgs, config, ...}:
let
  absolutePgData = "$(${pkgs.coreutils}/bin/realpath ${config.env.PGDATA})";
  dbString = "postgresql:///$USER?host=${absolutePgData}";
  chainweb-data-with-conn-params = pkgs.writeShellScript "cwd-with-conn-params" ''
    ${pkgs.chainweb-data}/bin/chainweb-data \
      --dbstring "${dbString}" --service-host localhost "$@"
  '' ;
  start-chainweb-data = pkgs.writeShellScript "start-chainweb-data" ''
    ${chainweb-data-with-conn-params} --migrate server --port 1849 --serve-swagger-ui
  '';
  psql-cwd = pkgs.writeShellScriptBin "psql-cwd" ''
    ${pkgs.postgresql}/bin/psql "${dbString}"
  '';
  chainweb-data-fill = pkgs.writeShellScriptBin "chainweb-data-fill" ''
    ${chainweb-data-with-conn-params} fill --level debug
  '';
in
{
  config = {
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

        - [Open API Spec](/cwd-spec/)
        - [DB Access](/ttyd/psql-cwd/)
        - [Run `fill` operation](/ttyd/chainweb-data-fill/)
      '';
    };
    sites.landing-page.commands.chainweb-data.markdown = ''
      * `psql-cwd`: Start a `psql` session as the `chainweb-data` service.
    '';
  };
}