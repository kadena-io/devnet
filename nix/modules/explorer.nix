{pkgs, lib, config, ...}:
with lib;
let
  cfg = config.sites.explorer;
  setConfigValues = builtins.toFile "setConfigValues.js" ''
    <script>
      function setConfig(key, value) {
        var node = document.querySelector('script[data-obelisk-executable-config-inject-key="' + key + '"]');
        node.innerHTML = btoa(value);
      }
      origin = window.location.origin;
      dataBackends = { development: { p2p: origin, service: origin, data: origin } };
      setConfig('common/route', 'origin');
      setConfig('frontend/data-backends', JSON.stringify(dataBackends));
    </script>
  '';
  processedIndex = pkgs.runCommand "processed-index.html.mustache" {} ''
    abspath=$(basename ${cfg.package}/ghcjs/*-all.js)

    mkdir -p $out

    sed \
      -e "s|/ghcjs/all.js|ghcjs/$abspath|g" \
      -e 's|<base data-ssr="" href="/" />|<base data-ssr="" href="/explorer/"/>|g' \
      -e 's|</head>|\n -script-placeholder- \n</head>|g' \
        ${cfg.package}/index.html.mustache > $out/index.html

    sed -e '/-script-placeholder-/ {' -e 'r ${setConfigValues}' -e 'd' -e '}' \
      -i $out/index.html
  '';
in {
  options.sites.explorer = {
    enable = mkEnableOption "Block Explorer";
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.block-explorer;
      defaultText = lib.literalExpression "pkgs.block-explorer";
      description = "The block-explorer bundle to use.";
    };
  };
  config = lib.mkIf cfg.enable {
    services.http-server.servers.devnet.extraConfig = ''
      location ~ ^/explorer/?$ {
          default_type text/html;
          return 200 '<html><head><meta http-equiv="refresh" content="0;url=/explorer/development" /></head></html>';
      }

      location /explorer/ {
        alias ${cfg.package}/;
        try_files $uri $uri/ @rewrite_index;

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
      location @rewrite_index {
        root ${processedIndex};
        rewrite ^ /index.html break;
        default_type text/html;

        # Disable caching
        add_header Cache-Control "no-cache, no-store, must-revalidate";
        add_header Pragma "no-cache";
      }
    '';
    sites.landing-page.services.block-explorer = {
      order = 5;
      markdown = ''
        ### Block Explorer ${config.lib.packageVersionInfoMd cfg.package}

        This container has [a block explorer](/explorer/) connected to the devnet.
      '';
    };
  };
}
