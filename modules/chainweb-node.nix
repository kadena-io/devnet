{ pkgs, config, ... }:
let
  chainweb-node = pkgs.chainweb-node;
  start-chainweb-node = stateDir: pkgs.writeShellScript "start-chainweb-node" ''
    ${chainweb-node}/bin/chainweb-node \
    --config-file=${./chainweb/chainweb-node.common.yaml} \
    --p2p-certificate-chain-file=${./chainweb/devnet-bootstrap-node.cert.pem} \
    --p2p-certificate-key-file=${./chainweb/devnet-bootstrap-node.key.pem} \
    --p2p-hostname=bootstrap-node \
    --bootstrap-reachability=2 \
    --cluster-id=devnet-minimal \
    --p2p-max-session-count=3 \
    --mempool-p2p-max-session-count=3 \
    --known-peer-info=YNo8pXthYQ9RQKv1bbpQf2R5LcLYA3ppx2BL2Hf8fIM@bootstrap-node:1789 \
    --log-level=info \
    --enable-mining-coordination \
    --mining-public-key=f90ef46927f506c70b6a58fd322450a936311dc6ac91f4ec3d8ef949608dbf1f \
    --header-stream \
    --rosetta \
    --allowReadsInLocal \
    --database-directory=${stateDir}/chainweb/db \
    --disable-pow
  '';
in
{
  config = {
    packages = [ chainweb-node ];
    processes.chainweb-node = {
      exec = "${start-chainweb-node config.env.DEVENV_STATE}";
      process-compose.readiness_probe = {
          http_get = {
          host = "127.0.0.1";
          scheme = "http";
          port = 1848;
          path = "/health-check";
          };
          initial_delay_seconds = 5;
          period_seconds = 10;
          timeout_seconds = 30;
          success_threshold = 1;
          failure_threshold = 10;
      };
    };
    # Work around a process-compose log display bug, remove these lines
    # once we start using a process-compsoe with the following merged:
    # https://github.com/F1bonacc1/process-compose/pull/74 is merged
    processes.chainweb-node.process-compose.disable_ansi_colors = true;

  };  
}