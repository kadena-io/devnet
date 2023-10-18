{pkgs, lib, config, ...}:
with lib;
let
  cfg = config.sites.explorer;
  lua_core_path = "${pkgs.luajitPackages.lua-resty-core}/lib/lua/5.1/?.lua";
  lua_lrucache_path = "${pkgs.luajitPackages.lua-resty-lrucache}/lib/lua/5.1/?.lua";
  lua_path = "${lua_core_path};${lua_lrucache_path};;";
  processedIndex = pkgs.runCommand "processed-index.html.mustache" {} ''
    abspath=$(basename ${cfg.package}/ghcjs/*-all.js)

    mkdir -p $out

    sed \
      -e "s|/ghcjs/all.js|ghcjs/$abspath|g" \
      -e 's|<base data-ssr="" href="/" />|<base data-ssr="" href="/explorer/"/>|g' \
        ${cfg.package}/index.html.mustache > $out/index.html.mustache
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
    services.http-server.nginx-modules = [ pkgs.nginxModules.lua ];
    services.http-server.extraHttpConfig = ''
      lua_package_path "${lua_path}";
    '';
    services.http-server.servers.devnet.extraConfig = ''
      location ~ ^/explorer/?$ {
          add_header Content-Type text/html;
          return 200 '<html><head><meta http-equiv="refresh" content="0;url=/explorer/fast-development" /></head></html>';
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
        rewrite ^ /index.html.mustache break;
        set $route ''';
        set $databackends ''';
        rewrite_by_lua_block {
          -- Derive the route from the request
          local route = ngx.var.scheme .. "://" .. ngx.req.get_headers()["Host"]

          -- Base64 encode the route
          local encoded = ngx.encode_base64(route)

          ngx.var.route = encoded

          local databackends = string.format([[
            {
              "fast-development": {
                "p2p": "%s",
                "service": "%s",
                "data": "%s"
              }
            }
          ]], route, route, route)

          ngx.var.databackends = ngx.encode_base64(databackends)
        }
        default_type text/html;
        sub_filter '{{route}}' $route;
        sub_filter '{{dataBackends}}' $databackends;
        sub_filter_once off;
        sub_filter_types *;

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