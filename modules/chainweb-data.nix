{ pkgs, config, ...}:
let
  absolutePgData = "$(${pkgs.coreutils}/bin/realpath ${config.env.PGDATA})";
  dbString = "postgresql:///$USER?host=${absolutePgData}";
  start-chainweb-data = pkgs.writeShellScript "start-chainweb-data" ''
    ${pkgs.chainweb-data}/bin/chainweb-data \
      --dbstring "${dbString}" \
      --service-host localhost --p2p-host localhost --p2p-port 1789 \
      --migrate server --port 1849 --serve-swagger-ui
  '';
  psql-cwd = pkgs.writeShellScriptBin "psql-cwd" ''
    ${pkgs.postgresql}/bin/psql "${dbString}"
  '';
in
{
  config = {
    packages = [ pkgs.chainweb-data psql-cwd ];

    processes.chainweb-data = {
      exec = "${pkgs.expect}/bin/unbuffer ${start-chainweb-data}";
      process-compose.depends_on = {
        chainweb-node.condition = "process_healthy";
        postgres.condition = "process_healthy";
      };
    };
    processes.psql-cwd = {
      exec = "${pkgs.ttyd}/bin/ttyd ${psql-cwd}/bin/psql-cwd";
      process-compose.depends_on.postgres.condition = "process_healthy";
    };

    services.postgres.enable = true;
    services.http-server = {
      upstreams.chainweb-data = "server localhost:1849;";
      upstreams.ttyd-psql-cwd = "server localhost:7861;";
      servers.devnet.extraConfig = ''
        location ~ /(stats$|coins$|cwd-spec|txs|richlist.csv$) {
          proxy_pass http://chainweb-data;
        }
        location ~ ^/psql-cwd(.*)$ {
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "upgrade";
            proxy_pass http://127.0.0.1:7681/$1;
        }
      '';
    };
  };
}