{ pkgs
, lib
, config
, ...
}:
with lib;
let cfg = config.services.postgres;
in {
  # We're injecting options into the postgresql service here in addition to ones from devenv.
  options.services.postgres = {
    forward-socket-port = mkOption {
      type = types.nullOr types.int;
      default = null;
      description = "The port to forward the postgresql socket to.";
    };
    remove-lock-files = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Remove the postgresql lock files on startup.
      '';
    };
  };
  config = mkIf cfg.enable {
    services.postgres = {
      package = pkgs.postgresql_14;
      extensions = extensions: with extensions; lists.flatten [
        (optional pkgs.stdenv.isLinux plv8)
      ];
    };

    init.devnet-init = optionalString (cfg.remove-lock-files) ''
      rm -f ${config.env.DEVENV_STATE}/postgres/postmaster.pid
      rm -f ${config.env.DEVENV_STATE}/postgres/.s.PGSQL.5432.lock
    '';
  };
  imports = [
    ( mkIf (cfg.enable && cfg.forward-socket-port != null) {
      processes.socat.exec = ''
        ${pkgs.socat}/bin/socat TCP-LISTEN:5432,reuseaddr,fork \
          UNIX-CONNECT:${config.env.PGDATA}/.s.PGSQL.5432
      '';
      sites.landing-page.container-api.ports = mkAfter
        "- `5432`: Postgresql";
    })
  ];
}

