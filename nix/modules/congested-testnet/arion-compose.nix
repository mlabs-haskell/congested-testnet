{ pkgs, ... }:
let
  cardanoSocket-relay="node-relay-1-socket";
  cardanoData-relay="node-relay-1-data";
  cardanoSocket-spo="node-spo-1-socket";
  cardanoData-spo="node-spo-1-data";
in
{
  project.name = "congested-testnet";
  docker-compose.raw.volumes = {
  "${cardanoSocket-relay}" = {};
  "${cardanoData-relay}" = {};
  "${cardanoSocket-spo}" = {};
  "${cardanoData-spo}" = {};
  };
  # services.prometheus = {
  #   # image = "prom/prometheus:v2.43.1";
  #   # pots = "9090:9090";
  #   service.image = "crccheck/hello-world";
  #   service.ports = [ "80:8000" ];
  # };

  # services.faucet = {
  #      image.enableRecommendedContents = true;
  #      service.useHostStore = true;
  #      service.command = [ "sh" "-c" ''${pkgs.congested.faucet}/bin/faucet''];
  #      service.ports = [
  #        "8000:8000" 
  #      ];
  #      # service.environment.WEB_ROOT = "${pkgs.nix.doc}/share/doc/nix/manual";
  #      service.stop_signal = "SIGINT";
  #    };

  services = { 


  node-relay-1 = {
    image.enableRecommendedContents = true;
    service = {
      useHostStore = true;
      command = [
        "sh"
        "-c"
        ''${pkgs.congested.cardano-node}/bin/cardano-node run  \
          --config                          ${pkgs.congested.testnet-conf}/configuration.yaml \
          --topology                        ${pkgs.congested.testnet-conf}/topology-relay-1.json \
          --database-path                   /data/db \
          --socket-path                     /socket/node.socket \
          --port                            3000
       ''
      ];
      ports = [
        "3000:3000"
      ];
      volumes = [
        "${cardanoSocket-relay}:/socket"
        "${cardanoData-relay}:/data"
      ];
    };
  };

  node-spo-1 = {
    image.enableRecommendedContents = true;
    service = {
      useHostStore = true;
      command = [
        "sh"
        "-c"
        ''${pkgs.congested.cardano-node}/bin/cardano-node run  \
          --config                          ${pkgs.congested.testnet-conf}/configuration.yaml \
          --topology                        ${pkgs.congested.testnet-conf}/topology-spo-1.json \
          --database-path                   /data/db \
          --socket-path                     /socket/node.socket \
          --port                            3000 \
          --shelley-kes-key                 ${pkgs.congested.testnet-conf}/configuration.yaml \ 

       ''
                  # --topology                        '/config/topology-spo-1.json' \
                  # --database-path                   'db' \
                  # --shelley-kes-key                 '/config/node-spo1/kes.skey' \
                  # --shelley-vrf-key                 '/config/node-spo1/vrf.skey' \
                  # --byron-delegation-certificate    '/config/node-spo1/byron-delegation.cert' \
                  # --byron-signing-key               '/config/node-spo1/byron-delegate.key' \
                  # --shelley-operational-certificate '/config/node-spo1/opcert.cert' \
                  # --config                          '/config/configuration.yaml' \
                  # --port                            '3000'"
      ];
      ports = [
        "3000:3000"
      ];
      volumes = [
        "${cardanoSocket-spo}:/socket"
        "${cardanoData-spo}:/data"
      ];
    };
  };
  };

}
