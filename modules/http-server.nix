{pkgs, ...}:
let
in {
  config = {
    services.nginx.enable = true;
    services.nginx.httpConfig = ''
    # Service API endpoints
    upstream service-api {
      server localhost:1848;
    }

    # Mining endpoints
    upstream mining-api {
      ip_hash; # for work and solve we need sticky connections
      server localhost:1848;
    }

    # P2P endpoints
    upstream peer-api {
      server localhost:1789;
    }

    server {
      # server_name api.devnet.chainweb.com
      listen 1337;

      # access_log /var/log/nginx/chainweb-api-access.log;
      # error_log /var/log/nginx/chainweb-api-error.log;

      # Service API endpoints
      location = /info {
          proxy_pass http://service-api;
      }
      location = /health-check {
          proxy_pass http://service-api;
      }
      location ~ ^/chainweb/0.0/[0-9a-zA-Z\-\_]+/chain/[0-9]+/pact/ {
          proxy_pass http://service-api;
      }
      location ~ ^/chainweb/0.0/[0-9a-zA-Z\-\_]+/chain/[0-9]+/(header|hash|branch|payload) {
          proxy_pass http://service-api;
      }
      location ~ /chainweb/0.0/[0-9a-zA-Z\-\_]+/cut {
          proxy_pass http://service-api;
      }

      # Optional Service APIs
      location ~ ^/chainweb/0.0/[0-9a-zA-Z\-\_]+/rosetta/ {
          proxy_pass http://service-api;
      }
      location ~ /chainweb/0.0/[0-9a-zA-Z\-\_]+/header/updates {
          proxy_buffering off;
          proxy_pass http://service-api;
      }

      # Mining
      location /chainweb/0.0/[0-9a-zA-Z\-\_]+/mining/ {
          proxy_buffering off;
          proxy_pass http://mining-api;
      }

      # Config (P2P API)
      location = /config {
          proxy_pass https://peer-api;
          # needed if self signed certificates are used for nodes:
          # proxy_ssl_verify off;
      }

      # Default
      location / {
          return 404;
      }
    }
    '';
  };
}