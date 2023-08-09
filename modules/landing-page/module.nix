{pkgs, lib, config, ...}:
let
  cfg = config.sites.landing-page;
  landing-page-root = pkgs.runCommand "landing-page" {} ''
    mkdir -p $out
    ${pkgs.pandoc}/bin/pandoc --template=${./index.html} -s ${./index.md} -o $out/index.html
    ln -s ${./static} $out/static
  '';
in
{
  # The following should be a NixOS module option containing a path to a directory containing index.md, index.html, and static/
  options.sites.landing-page.root = lib.mkOption {
    type = lib.types.package;
    default = landing-page-root;
    description = "The path to the compiled landing page folder.";
  };

  config = {
    services.http-server.servers.devnet.extraConfig = ''
      location / {
        alias ${cfg.root}/;
      }
    '';
  };
}