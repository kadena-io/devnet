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
    inputs.flake-utils.lib.eachDefaultSystem (system: let
      overlay = (self: super: {
        chainweb-data = bundle inputs.chainweb-data.packages.${system}.default;
        chainweb-mining-client = bundle inputs.chainweb-mining-client.packages.${system}.default;
        chainweb-node = bundle inputs.chainweb-node.packages.${system}.default;
        chainweb-node-l2 = bundle inputs.chainweb-node-l2.packages.${system}.default;
      });
      pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
      bundle = pkgs.callPackage inputs.nix-exe-bundle {};
      modules = [
        modules/chainweb-data.nix
        modules/chainweb-node.nix
        modules/chainweb-mining-client.nix
        modules/http-server.nix
        modules/ttyd.nix
        modules/landing-page/module.nix
        ({config, ...}: {
          services.ttyd.enable = true;
          # https://devenv.sh/reference/options/
          process.implementation = "process-compose";
          devenv.root = ".";
        })
      ];
      mkFlake = extraModules:
        import ./mkDevnetFlake.nix {
          inherit pkgs nixpkgs devenv;
          modules = modules ++ extraModules;
        };
      combined-flake = import lib/combine-flakes.nix pkgs.lib {
        default = mkFlake [];
        l2 = mkFlake [({pkgs, ...}: {
          services.chainweb-node.package = pkgs.chainweb-node-l2;
        })];
      };
      in pkgs.lib.recursiveUpdate combined-flake {
        apps.develop-page = {
          type = "app";
          program = (import ./lib/develop-page.nix {inherit pkgs;}).outPath;
        };
      });
}
