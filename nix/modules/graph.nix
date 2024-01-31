{ pkgs, config, ...}:
let
  absolutePgData = "$(${pkgs.coreutils}/bin/realpath ${config.env.PGDATA})";
  dbstring = "postgresql://$USER@localhost/$USER?host=${absolutePgData}";
in {
  config = {
    packages = [pkgs.kadena-graph pkgs.openssl];
    processes.graph.exec = ''
      DATABASE_URL="${dbstring}" ${pkgs.kadena-graph}/bin/kadena-graph
    '';
    services.http-server.servers.devnet.extraConfig = ''
      location = /graphql {
        proxy_pass http://localhost:4000;
      }
    '';
    sites.landing-page.services.graph = {
      order = 8;
      markdown = ''
        ### Graph
        * [GraphiQL](/graphql)
      '';
    };

  };
}