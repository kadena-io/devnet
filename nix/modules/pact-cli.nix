{pkgs, lib, config, ...}:
with lib;
let cfg = config.services.pact-cli;
in
{
  options.services.pact-cli = {
    enable = mkEnableOption "pact-cli";
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.pact;
      defaultText = lib.literalExpression "pkgs.pact";
      description = "The pact package to use.";
    };
    working-directory = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "The directory to use as working directory.";
    };
  };
  config = mkIf cfg.enable {
    packages = [ cfg.package ];
    services.ttyd.commands.pact-cli = (pkgs.writeShellScript "pact-cli-wrapper" ''
      ${optionalString (cfg.working-directory != null) "cd ${cfg.working-directory}"}
      ${cfg.package}/bin/pact
    '').outPath;
    sites.landing-page.services.pact-cli = {
      order = 20;
      markdown = ''
        ### Pact CLI ${config.lib.packageVersionInfoMd cfg.package}

        This container includes a pact interpreter accessible through your browser.

        - [Pact CLI](/ttyd/pact-cli/)
      '';
    };
    sites.landing-page.commands.pact-cli.markdown = ''
      * `pact`: Run the Pact interpreter.
    '';
    sites.landing-page.container-api.
      ${if cfg.working-directory == null then null else "folders"} = mkAfter
        "- `${cfg.working-directory}`: The working directory of the `pact-cli` terminal";
  };
}