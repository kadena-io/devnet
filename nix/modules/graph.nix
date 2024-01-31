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
  config = {
    packages = [cfg.package pkgs.openssl];
    processes.graph.exec = ''
      DATABASE_URL="${dbstring}" \
      PORT=${port} \
      ${cfg.package}/bin/kadena-graph
    '';
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
        * [GraphQL](/graphql)
      '';
    };
    sites.landing-page.container-api.ports =
      "- `${port}`: Graph service port";
  };
}