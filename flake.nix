{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    systems.url = "github:nix-systems/default";
    devenv.url = "github:cachix/devenv";
    chainweb-node.url = "github:kadena-io/chainweb-node";
    chainweb-data.url = "github:kadena-io/chainweb-data";
    kadena-js.url = "github:kadena-community/kadena.js";
    kadena-js.flake = false;
  };

  nixConfig = {
    extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs = { self, nixpkgs, devenv, systems, ... } @ inputs:
    let
      forEachSystem = nixpkgs.lib.genAttrs (import systems);
    in
    {
      devShells = forEachSystem
        (system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
          in
          {
            default = devenv.lib.mkShell {
              inherit inputs pkgs;
              modules = [
                {
                  # https://devenv.sh/reference/options/
                  packages = [ 
                    inputs.chainweb-node.packages.${system}.default
                    # inputs.chainweb-data.packages.${system}.default
                    # inputs.kadena-js 
                    nixpkgs.legacyPackages.${system}.nodejs-18_x
                  ];

                  enterShell = ''
                    npm install --global @microsoft/rush
                    cd ${inputs.kadena-js}
                    rush install --to @kadena/kda-cli
                    rush build --to @kadena/kda-cli
                    chmod +x ./lib/index.js
                    # if you are using NVM, you should have this environment variable available
                    ln -s $(pwd)/lib/index.js $NVM_BIN/kda
                    # if not, you can replace $NVM_BIN to any path you have added in your $PATH
                  '';
                }
              ];
            };
          });
    };
}
