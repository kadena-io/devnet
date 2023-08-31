{pkgs, lib, config, ...}:
with lib;
let
  renderInfo = package: let
    versionInfo = if package.version or null != null
      then "v${package.version}"
      else null;
    commitInfo = let
      info = package.flakeInfo or null;
      ref = info.ref or null;
      shortRev = info.shortRev or null;
      in if ref != null || shortRev != null
        then "${concatStringsSep "@" (builtins.filter (s: s!=null) [ref shortRev])}"
        else null
      ;
    packageInfo = concatStringsSep ", " (builtins.filter (s: s!=null) [versionInfo commitInfo]);
    withRevLink = str: if package.flakeInfo.revLink or null != null
      then "[${str}](${package.flakeInfo.revLink})"
      else str;
  in if packageInfo != ""
      then "<sup><small><small>${withRevLink packageInfo}</small></small></sup>"
      else "";
  mockPackage = {
    version = "1.2.3";
    flakeInfo = {
      revLink = "https://example.com";
      ref = "mock-branch";
      shortRev = "1234567";
    };
  };
  mode = config.devnet.mode;
  isDev = mode == "dev";
  isProd = mode == "prod";
in
{
  options = {
    devnet.mode = mkOption {
      type = types.enum [ "dev" "prod" ];
      default = "prod";
      description = "The mode in which devnet should be build. Can be either \"dev\" or \"prod\".";
    };
    lib.packageVersionInfoMd = mkOption {
      type = types.functionTo types.str;
      description = "Render the package version info as markdown.";
    };
  };
  config = {
    lib.packageVersionInfoMd = package: if isProd
      then renderInfo package
      else renderInfo mockPackage;
  };
}