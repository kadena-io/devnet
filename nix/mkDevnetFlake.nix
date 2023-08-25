{ pkgs
, nixpkgs
, devenv
, modules
, containerExtras
, packageExtras
}:
let
  mkDevnet = devnetModules: (devenv.lib.mkShell {
    inherit pkgs;
    modules = devnetModules;
    inputs = { inherit nixpkgs;};
  });
  mkRunner = devnet: pkgs.writeShellScript "start-devnet" ''
    export $(${pkgs.findutils}/bin/xargs < ${devnet.config.procfileEnv})
    ${devnet.config.procfileScript}
  '';
  packageDevnet = mkDevnet (modules ++ [packageExtras]);
  packageConfig = packageDevnet.config;
  packageRunner = mkRunner packageDevnet;

  containerDevnet = mkDevnet (modules ++ [containerExtras]);
  containerConfig = containerDevnet.config;
  containerRunner = mkRunner containerDevnet;
  nixConf = pkgs.writeTextDir "/etc/nix/nix.conf" ''
    experimental-features = nix-command flakes
  '';
  baseImage = pkgs.dockerTools.buildImageWithNixDb {
    name = "devnet-base";
    copyToRoot = pkgs.buildEnv {
      name = "devnet-base-root";
      paths = [
        containerConfig.services.nginx.package
        pkgs.chainweb-node
        pkgs.chainweb-mining-client
        pkgs.coreutils
        pkgs.findutils
        pkgs.bashInteractive
        pkgs.su
        pkgs.dockerTools.caCertificates
        pkgs.nix
        nixConf
      ];
    };
    runAsRoot = ''
      #!${pkgs.runtimeShell}
      ${pkgs.dockerTools.shadowSetup}

      mkdir /tmp
      chmod 777 /tmp

      # Nginx needs a nobody:nogroup
      groupadd -r nogroup
      useradd -r -g nogroup nobody

      groupadd -r devnet
      useradd -r -g devnet -d /devnet devnet

      mkdir /data
      chown devnet:devnet /data

      mkdir -p /devnet/.devenv
      ln -s /data /devnet/.devenv
      chown -R devnet:devnet /devnet

      mkdir /cwd-extra-migrations
    '';
  };
  packagesImage = pkgs.dockerTools.buildImage {
    name = "devnet-packages";
    fromImage = baseImage;
    copyToRoot = pkgs.buildEnv {
      name = "devnet-base-root";
      paths = containerConfig.packages;
    };
  };
  container = pkgs.dockerTools.buildImage {
    name = "devnet";
    fromImage = packagesImage;
    copyToRoot = pkgs.runCommand "start-devnet-bin" {} ''
      mkdir -p $out/bin
      ln -s ${containerRunner.outPath} $out/bin/start-devnet
    '';
    config = {
      WorkingDir = "/devnet";
      Cmd = [ "start-devnet" ];
      User = "devnet";
    };
  };
in
{
  packages = {
    default = packageRunner;
    container = container;
    landing-page = containerConfig.sites.landing-page.root.overrideAttrs (_:_:{
      allowSubstitutes = false;
    });
  };
  apps = {
    default = {
      type = "app";
      program = packageRunner.outPath;
    };
  };
  devShells.default = packageDevnet;
}