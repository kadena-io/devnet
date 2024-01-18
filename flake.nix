{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    devenv.url = "github:cachix/devenv";
    chainweb-node.url = "github:kadena-io/chainweb-node";
    chainweb-node-l2.url = "github:kadena-io/chainweb-node/edmund/l2-spv-poc";
    chainweb-data.url = "github:kadena-io/chainweb-data";
    chainweb-mining-client.url = "github:kadena-io/chainweb-mining-client/enis/update-to-flakes-and-haskellNix";
    pact.url = "github:kadena-io/pact";
    block-explorer.url = "github:kadena-io/block-explorer/devnet";
    nix-exe-bundle = { url = "github:3noch/nix-bundle-exe"; flake = false; };
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
        in pkgs.runCommand "${bundled.name}-bin" {} ''
          mkdir -p $out/bin
          ln -s ${bundled}/bin/* $out/bin
        '';
      get-flake-info = import nix/lib/get-flake-info.nix inputs;
      bundleWithInfo = inputs: let
        get-flake-info = import nix/lib/get-flake-info.nix inputs;
        in flakeName: let
          flakeInfo = get-flake-info flakeName;
          default = inputs.${flakeName}.packages.${system}.default;
        in bundle default // {
          inherit flakeInfo;
          version = default.version or default.meta.version or null;
        };
      bundleWithInfo' = bundleWithInfo inputs;
      overlay = (self: super: {
        chainweb-data = bundleWithInfo' "chainweb-data";
        chainweb-mining-client = bundleWithInfo' "chainweb-mining-client";
        chainweb-node = bundleWithInfo' "chainweb-node";
        pact = bundleWithInfo' "pact";
        block-explorer = inputs.block-explorer.packages.x86_64-linux.static // {
          flakeInfo = get-flake-info "block-explorer";
        };
      });
      chainweb-node-l2 = bundleWithInfo' "chainweb-node-l2";
      pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
      devnetInfo = {
        devnetVersion = "0.1.0";
        devnetRepo = "https://github.com/kadena-io/devnet";
        devnetRevision = self.rev or null;
      };
      modules = [
        # https://devenv.sh/reference/options/
        nix/modules/init.nix
        nix/modules/chainweb-data.nix
        nix/modules/chainweb-node.nix
        nix/modules/chainweb-mining-client.nix
        nix/modules/http-server.nix
        nix/modules/ttyd.nix
        nix/modules/landing-page/module.nix
        nix/modules/pact-cli.nix
        nix/modules/process-compose.nix
        nix/modules/devnet-mode.nix
        nix/modules/explorer.nix
        nix/modules/utils.nix
        { sites.landing-page = devnetInfo; }
      ];
      packageExtras = {
      };
      containerExtras = with pkgs.lib; {config, ...}:  {
        devenv.root = "/devnet";
        services.chainweb-data.extra-migrations-folder = "/cwd-extra-migrations";
        sites.landing-page.container-api.enable = true;
        imports = [
          (mkIf config.services.postgres.enable {
            processes.socat.exec = ''
              ${pkgs.socat}/bin/socat TCP-LISTEN:5432,reuseaddr,fork \
                UNIX-CONNECT:${config.env.PGDATA}/.s.PGSQL.5432
            '';
            sites.landing-page.container-api.ports = mkAfter
              "- `5432`: Postgresql";
          })
          (mkIf (config.services.pact-cli.enable && config.services.ttyd.enable) {
            services.pact-cli.working-directory = "/pact-cli";
            sites.landing-page.container-api.folders = mkAfter
              "- `/pact-cli`: The working directory of the `pact-cli` terminal";
          })
        ];
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
      configurations = let
        minimal = {
          services.chainweb-node.enable = true;
          services.chainweb-mining-client.enable = true;
          services.http-server.enable = true;
        };
        local = {
          imports = [minimal];
          services.chainweb-data.enable = true;
          sites.explorer.enable = true;
        };
        container-common = {
          imports = [local];
          services.ttyd.enable = true;
          services.pact-cli.enable = true;
        };
        use-cwn-l2 = {
          services.chainweb-node.package = chainweb-node-l2;
        };
        # Useful for iterating on nginx configurations
        http-only = {
          services.http-server.enable = true;
          # Keep process-compose alive even if nginx dies
          processes.sleep.exec = "sleep 100";
          sites.explorer.enable = true;
        };
      in {
        default = local;
        container-default = container-common;
        l2 = { imports = [container-common use-cwn-l2]; };
        minimal = minimal;
        inherit http-only;
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
        inherit configurations;
        overlays.default = overlay;
        lib = { inherit mkFlake bundleWithInfo; };
      });
}
