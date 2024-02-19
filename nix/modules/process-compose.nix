{pkgs, lib, config, ...}:
with lib;
let
  process-compose = config.process-managers.process-compose.package;
  pc-port = toString config.process-managers.process-compose.settings.port;
  attach = pkgs.writeShellScriptBin "attach" ''
    exec ${process-compose}/bin/process-compose attach -p ${pc-port}
  '';
in
{
  imports = [
    (lib.mkIf config.services.ttyd.enable {
      services.ttyd.commands.process-compose =
        "${attach}/bin/attach";
      sites.landing-page.main-links =
        "- [Process Manager](/ttyd/process-compose/)";
    })
  ];
  config = {
    packages = [ process-compose attach ];

    process.implementation = "process-compose";
    sites.landing-page.commands.process-compose.markdown =
      "* `attach`: Attach to the Process Compose management interface";
    sites.landing-page.container-api.ports = mkAfter
      "- `${pc-port}`: Process Compose management API";
  };
}
