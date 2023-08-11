{ pkgs
}:
let
  update = pkgs.writeShellScript "update-page" ''
    set -euo pipefail
    RESULT=$(
      nix build .#"$1" --no-link --print-out-paths \
        --option warn-dirty 'false'
    )
    ${pkgs.rsync}/bin/rsync -a --delete --chmod=ugo+rwX $RESULT/ "$2/"
  '';
  runBuild = pkgs.writeShellScript "run-build-page" ''
    set -euo pipefail
    ${update} "$1" "$2"
    ${pkgs.nodePackages.browser-sync}/bin/browser-sync reload
  '';
in pkgs.writeShellScript "develop-page" ''
  set -euo pipefail

  TMP_DIR="$(${pkgs.coreutils}/bin/mktemp -d -t "$1"_XXXXXX)"

  ${update} "$1" "$TMP_DIR"

  ${pkgs.nodePackages.browser-sync}/bin/browser-sync start --server "$TMP_DIR" &
  BROWSER_SYNC_PID=$!

  # Function to cleanup on exit
  cleanup() {
    ${pkgs.coreutils}/bin/kill $BROWSER_SYNC_PID || true
    ${pkgs.coreutils}/bin/rm -rf "$TMP_DIR"
  }
  trap cleanup EXIT

  ${pkgs.watchexec}/bin/watchexec --on-busy-update restart -- ${runBuild} "$1" "$TMP_DIR"
''