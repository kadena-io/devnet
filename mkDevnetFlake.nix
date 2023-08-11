{ pkgs
, nixpkgs
, devenv
, modules
}:
let
  devnet = devenv.lib.mkShell {
    inherit pkgs modules;
    inputs = { inherit nixpkgs;};
  };
  config = devnet.config;
  runner = pkgs.writeShellScript "start-devnet" ''
    export $(${pkgs.findutils}/bin/xargs < ${config.procfileEnv})
    ${config.procfileScript}
  '';
  nixConf = pkgs.writeTextDir "/etc/nix/nix.conf" ''
    experimental-features = nix-command flakes
  '';
  baseImage = pkgs.dockerTools.buildImageWithNixDb {
    name = "devnet-base";
    copyToRoot = pkgs.buildEnv {
      name = "devnet-base-root";
      paths = [
        config.services.nginx.package
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
      mkdir -p /devnet/.devenv
      chown -R devnet:devnet /devnet
    '';
  };
  packagesImage = pkgs.dockerTools.buildImage {
    name = "devnet-packages";
    fromImage = baseImage;
    copyToRoot = pkgs.buildEnv {
      name = "devnet-base-root";
      paths = config.packages;
    };
  };
  container = pkgs.dockerTools.buildImage {
    name = "devnet";
    fromImage = packagesImage;
    copyToRoot = pkgs.runCommand "start-devnet-bin" {} ''
      mkdir -p $out/bin
      ln -s ${runner.outPath} $out/bin/start-devnet
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
    default = runner;
    container = container;
    landing-page = config.sites.landing-page.root.overrideAttrs (_:_:{
      allowSubstitutes = false;
    });
  };
  apps = {
    default = {
      type = "app";
      program = runner.outPath;
    };
  };
  devShells.default = devnet;
}