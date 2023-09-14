inputs:
let flakeLock = builtins.fromJSON (builtins.readFile "${inputs.self}/flake.lock");
in

flakeName:
let
  inputInfo = flakeLock.nodes.${flakeName} or null;

  inputRevision = inputs.${flakeName}.rev;

  flakeInfo = if inputInfo != null && inputInfo.locked.rev == inputRevision then
    rec {
      ref = inputInfo.original.ref or null;
      repoLink = if inputInfo.original.type == "github"
        then
          "https://github.com/${inputInfo.original.owner}/${inputInfo.original.repo}"
        else null
        ;
      refLink = if inputInfo.original.type == "github"
        then if ref != null then "${repoLink}/tree/${ref}" else repoLink
        else null
        ;
      revLink = if inputInfo.original.type == "github"
        then "${repoLink}/tree/${inputInfo.locked.rev}"
        else null
        ;
    }
  else
    {
      ref = null;
      repoLink = null;
      refLink = null;
      revLink = null;
    };
in
  flakeInfo // {
    rev = inputs.${flakeName}.rev;
    shortRev = inputs.${flakeName}.shortRev;
  }
