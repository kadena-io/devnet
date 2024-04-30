{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.11";
    kadena-nix = {
      url = "github:kadena-io/kadena-nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat.url = "github:kadena-io/flake-compat";
    devenv.url = "github:cachix/devenv";
    chainweb-node.url = "github:kadena-io/chainweb-node";
    chainweb-data.url = "github:kadena-io/chainweb-data";
    chainweb-mining-client.url = "github:kadena-io/chainweb-mining-client";
    block-explorer.url = "github:kadena-io/block-explorer/devnet";
    nix-exe-bundle = { url = "github:3noch/nix-bundle-exe"; flake = false; };
    process-compose = {
      url = "github:F1bonacc1/process-compose";
      inputs = { nixpkgs.follows = "nixpkgs"; flake-utils.follows = "flake-utils"; };
    };
  };

  outputs = { self
            , nixpkgs
            , devenv
            , ... } @ inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system: let
      bundle = drv:
        let bundled = pkgs.callPackage inputs.nix-exe-bundle {} drv;
            # We're exposing the bin contents in a new derivation because the bundling produces
            # a /lib folder with duplicate files across different bundles, which causes problems
            # while preparing the docker image root
            bundledBin = pkgs.runCommand "${bundled.name}-bin" {} ''
              mkdir -p $out/bin
              ln -s ${bundled}/bin/* $out/bin
            '';
        in bundledBin // {
          version = drv.version or drv.meta.version or null;
        };
      pact = let
        cwnDefault = inputs.chainweb-node.packages.${system}.default;
        pactMeta = cwnDefault.cached.meta.pact;
        pactSrc = pkgs.fetchgit { inherit (pactMeta.src) rev url hash; name = "source";};
        pactFlake = (import inputs.flake-compat { src = pactSrc; }).defaultNix;
        meta = {
          flakeInfo.rev = pactMeta.src.rev;
          flakeInfo.shortRev = builtins.substring 0 7 pactMeta.src.rev;
          version = pactMeta.version;
        };
      in bundle pactFlake.packages.${system}.default // meta;
      get-flake-info = import nix/lib/get-flake-info.nix inputs;
      bundleWithInfo = inputs: let
        get-flake-info = import nix/lib/get-flake-info.nix inputs;
        in flakeName: let
          flakeInfo = get-flake-info flakeName;
          default = inputs.${flakeName}.packages.${system}.default;
        in bundle default // { inherit flakeInfo; };
      bundleWithInfo' = bundleWithInfo inputs;
      overlay = (self: super: {
        chainweb-data = bundleWithInfo' "chainweb-data";
        chainweb-mining-client = bundleWithInfo' "chainweb-mining-client";
        chainweb-node = bundleWithInfo' "chainweb-node";
        pact = pact;
        block-explorer = inputs.block-explorer.packages.x86_64-linux.static // {
          flakeInfo = get-flake-info "block-explorer";
        };
        kadena-graph = let
          inherit (inputs.kadena-nix.packages.${system}) kadena-graph;
          inherit (kadena-graph) packageName version;
          flakeInfo.revLink = "https://npmjs.com/package/${packageName}/v/${version}";
          in kadena-graph // { inherit flakeInfo; }
          ;
        mining-trigger = pkgs.haskellPackages.callCabal2nix "mining-trigger" nix/pkgs/mining-trigger {
          scotty = pkgs.haskellPackages.scotty_0_20_1;
        };
      });
      pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
      devnetInfo = {
        devnetVersion = "0.1.0";
        devnetRepo = "https://github.com/kadena-io/devnet";
        devnetRevision = self.rev or null;
      };
      modules = [
        # https://devenv.sh/reference/options/
        nix/modules/init.nix
        nix/modules/postgres.nix
        nix/modules/chainweb-data.nix
        nix/modules/chainweb-node.nix
        nix/modules/chainweb-mining-client.nix
        nix/modules/http-server.nix
        nix/modules/ttyd.nix
        nix/modules/sqlpage.nix
        nix/modules/landing-page/module.nix
        nix/modules/pact-cli.nix
        nix/modules/process-compose.nix
        nix/modules/devnet-mode.nix
        nix/modules/explorer.nix
        nix/modules/utils.nix
        nix/modules/graph.nix
        {
          sites.landing-page = devnetInfo;
          process-managers.process-compose.package =
            inputs.process-compose.packages.${system}.process-compose;
        }
      ];
      packageExtras = {
      };
      containerExtras = with pkgs.lib; {config, ...}:  {
        devenv.root = "/devnet";
        services.chainweb-data.extra-migrations-folders = [ "/cwd-extra-migrations" ];
        sites.landing-page.container-api.enable = mkDefault true;
        services.postgres.forward-socket-port = mkDefault 5432;
        services.postgres.remove-lock-files = true;
        services.pact-cli.working-directory = mkDefault "/pact-cli";
        sites.landing-page.container-api.folders = mkBefore "- `/data`: Persistent data folder";
        init.container-init = ''
          mkdir /cwd-extra-migrations
          mkdir /pact-cli
        '';
        init.devnet-init = ''
          # Assert that these folders are mounted as read-only
          for dir in /cwd-extra-migrations /pact-cli; do
            dir_mount="^[^\S]+ $dir"
            (${pkgs.gnugrep}/bin/grep -E "$dir_mount" /proc/mounts || true) | while read -r mount; do
              is_readonly="$dir_mount [^\S]+ ro"
              if ! [[ $mount =~ $is_readonly ]]; then
                echo "Error: $dir should be mounted as read-only."
                exit 1
              fi
            done
          done
        '';
      };
      mkFlake = cfgName: cfgPos: extraModule:
        import nix/mkDevnetFlake.nix {
          inherit pkgs nixpkgs devenv containerExtras packageExtras;
          containerTag = cfgName;
          modules = modules ++ [
            extraModule
            {
              sites.landing-page = {
                devnetConfig = cfgName;
              } // pkgs.lib.optionalAttrs (cfgPos != null) {devnetConfigPos = cfgPos;};
            }
          ];
        };
      configurations = rec {
        minimal = {
          services.chainweb-node.enable = true;
          services.chainweb-mining-client.enable = true;
          services.http-server.enable = true;
        };
        default = {
          imports = [minimal];
          services.chainweb-data.enable = true;
          services.graph.enable = true;
          sites.explorer.enable = true;
        };
        crashnet = {
          imports = [default];
          services.postgres.forward-socket-port = null;
          services.chainweb-node.throttle = true;
          services.chainweb-data.throttle = true;
          services.chainweb-mining-client.expose-make-blocks = false;
        };
        container-default = {
          imports = [default];
          services.ttyd.enable = true;
          services.pact-cli.enable = true;
        };
        # Useful for iterating on nginx configurations
        http-only = {
          services.http-server.enable = true;
          # Keep process-compose alive even if nginx dies
          processes.sleep.exec = "sleep 100";
          sites.explorer.enable = true;
        };
      };
      mkCfgPos = cfgName:
        let pos = builtins.unsafeGetAttrPos cfgName configurations;
        in { file = pkgs.lib.strings.removePrefix "${self}/" pos.file; line = pos.line; };
      combined-flake = import nix/lib/combine-flakes.nix pkgs.lib (
        builtins.mapAttrs (cfgName: config: mkFlake cfgName (mkCfgPos cfgName) config) configurations
      );
      in pkgs.lib.recursiveUpdate combined-flake {
        apps.develop-page = {
          type = "app";
          program = (import nix/lib/develop-page.nix {inherit pkgs;}).outPath;
        };
        apps.develop-sqlpage = {
          type = "app";
          program = (pkgs.writeShellScript "develop-sqlpage" ''
            DEVNET_VARIANT="''${1:-container-default}"
            export SQLPAGE_PORT_OVERRIDE=8091
            ${pkgs.watchexec}/bin/watchexec --on-busy-update restart -- \
              nix run --impure ".#$DEVNET_VARIANT/runSqlpage"
          '').outPath;
        };
        packages.mining-trigger = pkgs.mining-trigger;
        inherit configurations;
        overlays.default = overlay;
        lib = { inherit mkFlake bundleWithInfo; };
      });
}
