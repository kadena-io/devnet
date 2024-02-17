{pkgs, lib, config, ...}:
with lib;
let
  process-compose = config.process-managers.process-compose.package;
  pc-port = toString config.process-managers.process-compose.settings.port;
in
{
  imports = [
    (lib.mkIf config.services.ttyd.enable {
      services.ttyd.commands.process-compose =
        "${process-compose}/bin/process-compose attach -p ${pc-port}";
      sites.landing-page.main-links =
        "- [Process Manager](/ttyd/process-compose/)";
    })
  ];
  config = {
    packages = [ process-compose ];

    process.implementation = "process-compose";
    sites.landing-page.container-api.ports = mkAfter
      "- `${pc-port}`: Process Compose management API";
  };
}
