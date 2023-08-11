lib:
variants:
let
  mergeSets = sets: (sets.default or {}) // builtins.listToAttrs (
    builtins.concatLists (
      lib.attrsets.mapAttrsToList (setName: set:
        lib.optional (set ? default) { name = setName; value = set.default;} ++
        lib.attrsets.mapAttrsToList (elemName: elem: {
          name = "${setName}/${elemName}";
          value = elem;
        }) (builtins.removeAttrs set ["default"])
      ) (builtins.removeAttrs sets ["default"])
    )
  );
in
{
  packages = mergeSets (builtins.mapAttrs (_: flake: flake.packages) variants);
  apps = mergeSets (builtins.mapAttrs (_: flake: flake.apps) variants);
  devShells = mergeSets (builtins.mapAttrs (_: flake: flake.devShells) variants);
}
