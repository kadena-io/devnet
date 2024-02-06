{ pkgs, config, lib, ...}:
with lib;
let
  cfg = config.services.graph;
  port = toString cfg.port;
  absolutePgData = "$(${pkgs.coreutils}/bin/realpath ${config.env.PGDATA})";
  dbstring = "postgresql://$(whoami)@localhost/$(whoami)?host=${absolutePgData}";
in {
  options.services.graph = {
    enable = mkEnableOption "graph";
    package = mkOption {
      type = types.package;
      default = pkgs.kadena-graph;
      description = ''
        The package to use for the graph service.
      '';
    };
    port = mkOption {
      type = types.int;
      default = 4000;
      description = ''
        The port on which the graph service will listen.
      '';
    };
  };
  config = mkIf cfg.enable {
    packages = [cfg.package pkgs.openssl];
    processes.graph = {
      # DATABASE_URL needs to be a part of the exec, because it interpolates the
      # username and the absolute path to the PGDATA directory.
      exec = ''DATABASE_URL="${dbstring}"  ${cfg.package}/bin/kadena-graph'';
      process-compose.environment = [
        "PORT=${port}"
        "PRISMA_LOGGING_ENABLED=true"
      ];
    };
    services.chainweb-data.extra-migrations-folders = [
      "${cfg.package}/lib/node_modules/@kadena/graph/cwd-extra-migrations/"
    ];
    services.http-server = {
      upstreams.graph = "server localhost:${port};";
      servers.devnet.extraConfig = ''
        location = /graphql {
          proxy_pass http://graph;
          proxy_buffering off;
        }
      '';
    };
    sites.landing-page.services.graph = {
      order = 8;
      markdown = ''
        ### Graph ${config.lib.packageVersionInfoMd cfg.package}

        Kadena GraphQL uses chainweb-data's database and various chainweb-node
        endpoints to provide a GraphQL interface to the Kadena blockchain data.
        The service is running at port `${port}`, however the HTTP API is configured
        to proxy the GraphQL endpoint at the `/graphql` route.

        * Interactive [GraphQL Yoga](/graphql) interface
      '';
    };
    sites.landing-page.container-api.ports =
      "- `${port}`: Graph service port";
  };
}
