{ pkgs
, nixpkgs
, devenv
, modules
, containerExtras
, packageExtras
, containerTag
}:
let
  mkDevnet = devnetModules: (devenv.lib.mkShell {
    inherit pkgs;
    modules = devnetModules;
    inputs = { inherit nixpkgs;};
  });
  mkRunner = devnet: pkgs.writeShellScript "start-devnet" ''
    # Check for TTY
    if [ -t 1 ]; then
        # Terminal is present, default to TUI for process-compose
        PC_TUI_ENABLED=''${PC_TUI_ENABLED:-1}
    else
        # Terminal is not present, default to no-TUI for process-compose
        PC_TUI_ENABLED=''${PC_TUI_ENABLED:-0}
    fi
    export PC_TUI_ENABLED

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

      mkdir -p /devnet
      ln -s /data /devnet/.devenv
      chown -R devnet:devnet /devnet

      mkdir /cwd-extra-migrations
      mkdir /pact-cli
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
    tag = containerTag;
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
  devConfig = (mkDevnet (modules ++ [
    containerExtras
    ({ devnet.mode = "dev"; })
  ])).config;
in
{
  packages = {
    default = packageRunner;
    container = container;
    landing-page = devConfig.sites.landing-page.root.overrideAttrs (_:_:{
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