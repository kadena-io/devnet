{ pkgs
, packageName
}:
let
  update = no-substituters: pkgs.writeShellScript "update-${packageName}" ''
    RESULT=$(
      nix build .#${packageName} --no-link --print-out-paths \
        --option warn-dirty 'false' \
        ${pkgs.lib.strings.optionalString no-substituters ''--substituters ""''}
    )
    ${pkgs.rsync}/bin/rsync -a --delete --chmod=ugo+rwX $RESULT/ $1/
  '';
  runBuild = pkgs.writeShellScript "run-build-${packageName}" ''
    ${update true} $1
    ${pkgs.nodePackages.browser-sync}/bin/browser-sync reload
  '';
in pkgs.writeShellScript "develop-${packageName}" ''
  set -euo pipefail

  TMP_DIR=$(${pkgs.coreutils}/bin/mktemp -d -t ${packageName}_XXXXXX)

  ${update false} $TMP_DIR

  ${pkgs.nodePackages.browser-sync}/bin/browser-sync start --server $TMP_DIR &
  BROWSER_SYNC_PID=$!

  # Function to cleanup on exit
  cleanup() {
    ${pkgs.coreutils}/bin/kill $BROWSER_SYNC_PID
    ${pkgs.coreutils}/bin/rm -rf $TMP_DIR
  }
  trap cleanup EXIT

  ${pkgs.watchexec}/bin/watchexec --on-busy-update restart -- ${runBuild} $TMP_DIR
''