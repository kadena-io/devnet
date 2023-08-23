{pkgs, lib, config, ...}:
with pkgs.lib;
let
  cfg = config.sites.landing-page;
  mkSection = subsections: concatStringsSep "\n" (
    map (section: section.markdown) (
      builtins.sort (a: b: a.order < b.order)
        (builtins.attrValues subsections)
    )
  );
  indexArgs = {
    services = mkSection cfg.services;
    commands = mkSection cfg.commands;
    containerApi = optionalString (cfg.container-api.enable) ''
      ## Container API

      ### Ports
      ${cfg.container-api.ports}

      ### Folders
      ${cfg.container-api.folders}
    '';
  };
  landing-page-root = pkgs.runCommand "landing-page" {} ''
    mkdir -p $out
    hash=$(basename $out | cut -d'-' -f1)
    ${pkgs.mustache-go}/bin/mustache \
      ${builtins.toFile "index-md-input.json" (builtins.toJSON indexArgs)} \
      ${./index.md.mustache} \
      > index.md

    ${pkgs.pandoc}/bin/pandoc \
      --template=${./index.html} \
      -s index.md \
      --variable store-hash=$hash \
      -o $out/index.html

    mkdir -p $out/static

    ${pkgs.xorg.lndir}/bin/lndir ${./static} $out/static

    mkdir -p $out/static/fonts
    for font in KodeMono-SemiBold; do
      cp ${pkgs.kode-mono}/share/fonts/truetype/$font.ttf $out/static/fonts
    done
  '';
  sectionsType = types.attrsOf (types.submodule {
    options = {
      order = mkOption {
        type = types.int;
        description = "Order of the subsection";
        default = 0;
      };
      markdown = mkOption {
        type = types.lines;
        description = "Content of the service subsection";
        default = "";
        example = ''
          ## My Service
          * [Service Link](/my-service/)
        '';
      };
    };
  });
in
{
  # The following should be a NixOS module option containing a path to a directory containing index.md, index.html, and static/
  options.sites.landing-page = {
    root = lib.mkOption {
      type = lib.types.package;
      default = landing-page-root;
      description = "The path to the compiled landing page folder.";
    };
    services = mkOption {
      default = {};
      type = sectionsType;
      description = "Subsections of the Services section.";
    };
    commands = mkOption {
      default = {};
      type = sectionsType;
      description = ''Subsections of the "Available Commands" section.'';
    };
    container-api = {
      enable = mkEnableOption "container-api-docs";
      ports = mkOption {
        type = types.lines;
        description = "Ports exposed by the container";
        default = "";
      };
      folders = mkOption {
        type = types.lines;
        description = "Special folders for interfacing with the container";
        default = "";
      };
    };
  };

  config = {
    services.http-server.servers.devnet.extraConfig = ''
      location / {
        alias ${cfg.root}/;

        types {
          text/html html htm shtml;
          text/css css;
          text/javascript js;
          image/jpeg jpg jpeg;
          image/png png;
          image/gif gif;
          image/svg+xml svg svgz;
          image/webp webp;
          image/x-icon ico;
          text/plain txt;
          text/xml xml;
          application/xml rss atom;
          application/json json;
          application/font-woff woff;
          application/font-woff2 woff2;
          application/vnd.ms-fontobject eot;
          application/x-font-ttf ttf;
          font/opentype otf;
          application/octet-stream bin;
        }
      }
    '';
  };
}