{pkgs, lib, config, ...}:
with pkgs.lib;
let
  cfg = config.sites.landing-page;
  concatAsLinesSep = sep: strings: builtins.concatStringsSep sep (map
    (str: if hasSuffix "\n" str then str else str + "\n")
    strings
  );
  mkSection = sep: subsections: concatAsLinesSep sep (
    map (section: section.markdown) (
      builtins.sort (a: b: a.order < b.order)
        (builtins.attrValues subsections)
    )
  );
  configDocLink = let
    pos = cfg.devnetConfigPos;
    configUrl = "${cfg.devnetRepo}/blob/${cfg.devnetRevision}/${pos.file}#L${toString pos.line}";
    in if all (x: x!=null) [ pos.file pos.line cfg.devnetRepo cfg.devnetRevision ]
      then "[the `${cfg.devnetConfig}` configuration](${configUrl})"
      else "the `${cfg.devnetConfig}` configuration";

  configDoc = optionalString (cfg.devnetConfig != null) ''
    This is ${configDocLink} of the Kadena Devnet.
  '';
  devnetPseudoPackage = {
    version = cfg.devnetVersion;
    flakeInfo = optionalAttrs (cfg.devnetRevision != null) {
       revLink = if (cfg.devnetRepo != null)
         then cfg.devnetRepo + "/tree/" + cfg.devnetRevision
         else null;
       shortRev = builtins.substring 0 7 cfg.devnetRevision;
    };
  };
  indexArgs = {
    services = mkSection "\n" cfg.services;
    commands = mkSection "" cfg.commands;
    containerApi = optionalString (cfg.container-api.enable) ''
      ## Container API

      ### Ports
      ${cfg.container-api.ports}

      ### Folders
      ${cfg.container-api.folders}

      ### Environment Variables
      ${cfg.container-api.envVars}
    '';
    mainSection = ''
      ${configDoc}

      ${cfg.main-links}
    '';
    devnetSource = config.lib.packageVersionInfoMd devnetPseudoPackage;
  };
  landing-page-root = pkgs.runCommand "landing-page" {} ''
    mkdir -p $out
    hash=$(basename $out | cut -d'-' -f1)
    ${pkgs.mustache-go}/bin/mustache \
      ${pkgs.writeText "index-md-input.json" (builtins.toJSON indexArgs)} \
      ${./index.md.mustache} \
      > index.md

    cp index.md $out/index.md

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
      envVars = mkOption {
        type = types.lines;
        description = "Environment variables used by the container's services";
        default = "";
      };
    };
    main-links = mkOption {
      type = types.lines;
      default = "";
      description = "The links in the main section of the landing page.";
    };
    devnetVersion = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "The optional devnet version to state in the main section.";
    };
    devnetConfig = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "The optional devnet variant to state in the main section.";
    };
    devnetConfigPos.file = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "The optional file name containing the devnet configuration.";
    };
    devnetConfigPos.line = lib.mkOption {
      type = types.nullOr types.int;
      default = null;
      description = "The optional line number of the devnet configuration.";
    };
    devnetRepo = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "The optional devnet repo.";
    };
    devnetRevision = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "The optional devnet revision.";
    };
  };

  config = {
    services.http-server.servers.devnet.extraConfig = ''
      location = / {
        root ${cfg.root}/;
        try_files /index.html =404;
        # Make sure the browser checks the ETag every time
        add_header Cache-Control "no-cache, must-revalidate";
      }
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