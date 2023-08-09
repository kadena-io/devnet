{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    devenv.url = "github:kadena-io/devenv/devnet-setup";
    chainweb-node.url = "github:kadena-io/chainweb-node/edmund/fast-devnet";
    chainweb-node-l2.url = "github:kadena-io/chainweb-node/edmund/l2-spv-poc";
    chainweb-data = {
      url = "github:kadena-io/chainweb-data/";
      inputs.nixpkgs.follows = "chainweb-node/nixpkgs";
      inputs.haskellNix.follows = "chainweb-node/haskellNix";
    };
    chainweb-mining-client = {
      url = "github:kadena-io/chainweb-mining-client/enis/update-to-flakes-and-haskellNix";
      inputs.haskellNix.follows = "chainweb-node/haskellNix";
      inputs.nixpkgs.follows = "chainweb-node/nixpkgs";
    };
    nix-exe-bundle = { url = "github:3noch/nix-bundle-exe"; flake = false; };
  };

  outputs = { self
            , nixpkgs
            , devenv
            , ... } @ inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let common-overlay = (self: super: {
            chainweb-data = bundle inputs.chainweb-data.packages.${system}.default;
            chainweb-mining-client = bundle inputs.chainweb-mining-client.packages.${system}.default;
          });
          l1-overlay = (self: super: {
            chainweb-node = bundle inputs.chainweb-node.packages.${system}.default;
          });
          l2-overlay = (self: super: {
            chainweb-node = bundle inputs.chainweb-node-l2.packages.${system}.default;
          });
          pkgs = nixpkgs.legacyPackages.${system};
          l1-pkgs = import nixpkgs { inherit system; overlays = [ common-overlay l1-overlay ]; };
          l2-pkgs = import nixpkgs { inherit system; overlays = [ common-overlay l2-overlay ]; };
          bundle = pkgs.callPackage inputs.nix-exe-bundle {};
      in rec {
        packages = rec {
          default = mkDevnetRunner { devnet = devShells.default; };
          l2 = mkDevnetRunner { devnet = devShells.l2; };
          container = mkContainer l1-pkgs;
          container-l2 = mkContainer l2-pkgs;
          cwd = inputs.chainweb-data.packages.default;
          landing-page = devShells.default.config.sites.landing-page.root.overrideAttrs (_:_:{
            allowSubstitutes = false;
          });
        };

        apps = {
          default = {
            type = "app";
            program = packages.default.outPath;
          };
          l2 = {
            type = "app";
            program = packages.l2.outPath;
          };
          develop-landing-page = {
            type = "app";
            program = (import ./lib/develop-page.nix {inherit pkgs; packageName = "landing-page";}).outPath;
          };
        };

        devShells = {
          default = mkDevnet { pkgs = l1-pkgs; };
          l2 = mkDevnet { pkgs = l2-pkgs; };
        };

        mkContainer = pkgs:
          let
            devnet = mkDevnet { inherit pkgs; };
            config = devnet.config;
            devnetRunner = mkDevnetRunner {inherit devnet;};
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
                paths = devnet.config.packages;
              };
            };
          in pkgs.dockerTools.buildImage {
            name = "devnet";
            fromImage = packagesImage;
            copyToRoot = pkgs.runCommand "start-devnet-bin" {} ''
              mkdir -p $out/bin
              ln -s ${devnetRunner.outPath} $out/bin/start-devnet
            '';
            config = {
              WorkingDir = "/devnet";
              Cmd = [ "start-devnet" ];
              User = "devnet";
            };
          };

        mkDevnetRunner = { devnet }:
          let config = devnet.config;
              pkgs = nixpkgs.legacyPackages.${system};
          in pkgs.writeShellScript "start-devnet" ''
            export $(${pkgs.findutils}/bin/xargs < ${config.procfileEnv})
            ${config.procfileScript}
          '';

        mkDevnet = args@{ pkgs }: devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [
            modules/chainweb-data.nix
            modules/chainweb-node.nix
            modules/chainweb-mining-client.nix
            modules/http-server.nix
            modules/landing-page/module.nix
            ({config, ...}: {
              # https://devenv.sh/reference/options/
              process.implementation = "process-compose";
              devenv.root = ".";
            })
          ];
        };
      }
    );
}
