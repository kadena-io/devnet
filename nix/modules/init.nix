{pkgs, lib, ...}:
with lib;
{
  options.init = {
    container-init = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Commands to run during the container build.
      '';
    };
    devnet-init = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Commands to run before starting up the devnet service processes.
      '';
    };
  };
}