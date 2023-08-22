{pkgs, lib, config, ...}:
with lib;
let cfg = config.services.pact-cli;
in
{
  options.services.pact-cli = {
    enable = mkEnableOption "pact-cli";
  };
  config = mkIf cfg.enable {
    packages = [ pkgs.pact ];
    services.http-server.servers.devnet.extraConfig = ''
      location /pact-cli/uploads {
        alias /tmp/uploads;
        dav_methods PUT DELETE MKCOL COPY MOVE;
        dav_access user:rw group:rw all:rw;
      }
    '';
    services.ttyd.commands.pact-cli = (pkgs.writeShellScript "pact-cli-wrapper" ''
      cd /tmp/uploads
      ${pkgs.pact}/bin/pact
    '').outPath;
    sites.landing-page.services.pact-cli = {
      order = 20;
      markdown = ''
        ### Pact CLI

        This container includes a pact interpreter accessible through your browser.

        - [Pact CLI](/ttyd/pact-cli/)
      '';
    };
    sites.landing-page.commands.chainweb-data.markdown = ''
      * `pact`: Run the Pact interpreter.
    '';
    processes.init-pact-cli.exec = (pkgs.writeShellScript "init-pact-cli" ''
      set -euxo pipefail

      mkdir -p /tmp/uploads
    '').outPath;
  };
}