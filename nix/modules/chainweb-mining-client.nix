{ pkgs, lib, config, ...}:
with lib;
let
  cfg = config.services.chainweb-mining-client;
  start-chainweb-mining-client = pkgs.writeShellScript "start-chainweb-mining-client" ''
    ${pkgs.chainweb-mining-client}/bin/chainweb-mining-client \
    --public-key=f89ef46927f506c70b6a58fd322450a936311dc6ac91f4ec3d8ef949608dbf1f \
    --node=127.0.0.1:1848 \
    --worker=on-demand \
    --on-demand-port=1790 \
    --thread-count=1 \
    --log-level=info \
    --no-tls
  '';
in
{
  options.services.chainweb-mining-client = {
    enable = mkEnableOption "Enable the chainweb-mining-client service";
  };
  config = mkIf cfg.enable {
    packages = [ pkgs.chainweb-mining-client ];
    processes.chainweb-mining-client = {
      exec = "${start-chainweb-mining-client}";
      process-compose = {
        depends_on.chainweb-node.condition = "process_healthy";
      };
    };
    services.http-server = {
      upstreams.chainweb-mining-client = "server localhost:1790;";
      servers.devnet.extraConfig = ''
        location = /make-blocks {
          proxy_pass http://chainweb-mining-client/make-blocks;
          proxy_buffering off;
        }
      '';
    };
    sites.landing-page.services.chainweb-mining-client = {
    order = 8;
    markdown = ''
      ### On-Demand Mining
      * [Make blocks](/make-blocks)
    '';
  };
  sites.landing-page.container-api.ports =
    "- `1790`: On-Demand Mining API";
  };
 
  
}
