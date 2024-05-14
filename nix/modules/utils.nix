# This module contains some CLI utilities that are useful to have in the container
{pkgs, lib, config, ...}:
with lib;
let cfg = config.utils;
    kadena-cli = cfg.kadena-cli.package;
    verInfo = config.lib.packageVersionInfoMd kadena-cli;
in {
  options.utils = {
    kadena-cli.enable = mkEnableOption "Enable the kadena-cli utility";
    kadena-cli.package = mkOption {
      type = types.package;
      default = pkgs.kadena-cli;
      defaultText = lib.literalExpression "pkgs.kadena-cli";
      description = "The kadena-cli package to use.";
    };
  };
  config = mkMerge [
    {
      packages = [
        pkgs.curl # Used by some CI automation
      ];
    }
    (mkIf cfg.kadena-cli.enable {
      packages = [ kadena-cli ];
      sites.landing-page.commands.kadena.markdown = ''
        * `kadena`: CLI to interact with Kadena and its ecosystem ${verInfo}
      '';
    })
  ];
}