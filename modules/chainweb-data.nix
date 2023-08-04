{ pkgs, config, ...}:
let
  absolutePgData = "$(${pkgs.coreutils}/bin/realpath ${config.env.PGDATA})";
  start-chainweb-data = pkgs.writeShellScript "start-chainweb-data" ''
    ${pkgs.chainweb-data}/bin/chainweb-data \
      --dbstring "postgresql:///$USER?host=${absolutePgData}" \
      --service-host localhost --p2p-host localhost --p2p-port 1789 \
      --migrate server --port 1849 --serve-swagger-ui
  '';
in
{
  config = {
    packages = [ pkgs.chainweb-data ];

    processes.chainweb-data = {
      exec = "${start-chainweb-data}";
      process-compose.depends_on = {
        chainweb-node.condition = "process_healthy";
        postgres.condition = "process_healthy";
      };
    };

    services.postgres.enable = true;
    services.http-server = {
      upstreams.chainweb-data = "server localhost:1849;";
      servers.devnet.extraConfig = ''
          location ~ /(stats$|coins$|cwd-spec|txs|richlist.csv$) {
            proxy_pass http://chainweb-data;
          }
        '';
      };
  };
}