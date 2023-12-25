{ pkgs, ... }:
let
  cardanoSocket-relay = "node-relay-1-socket";
  cardanoData-relay = "node-relay-1-data";
  cardanoSocket-spo = "node-spo-1-socket";
  cardanoData-spo = "node-spo-1-data";
  spo-port = 3001;
  relay-port = 3000;
  bindPort = port: "${toString port}:${toString port}";
in
{
  project.name = "testnet";
  networks.default.ipam.config = [{subnet = "192.168.224.0/20";}]; 

  docker-compose.volumes = {
      "${cardanoSocket-relay}" = {};
      "${cardanoData-relay}" = {};
      "${cardanoSocket-spo}" = {};
      "${cardanoData-spo}" = {};
  };

  # services.prometheus = {
  #   # image = "prom/prometheus:v2.43.1";
  #   # pots = "9090:9090";
  #   service.image = "crccheck/hello-world";
  #   service.ports = [ "8000:8000" ];
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
      # image.enableRecommendedContents = true;
      service = {
        image = "inputoutput/cardano-node:8.1.2";
        # useHostStore = true;
        networks.default.ipv4_address = "192.168.224.3";
        entrypoint = ''
         sh -c "cardano-node run --config /config/configuration.yaml --topology /config/topology-relay-1.json --database-path  /data/db --socket-path /socket/node.socket --port ${toString relay-port} & wait"
         '';
       #  command = [
       #    "sh"
       #    "-c"
       #    ''${pkgs.congested.cardano-node}/bin/cardano-node run  \
       #    --config                          /config/configuration.yaml \
       #    --topology                        /config/topology-relay-1.json \
       #    --database-path                   /data/db \
       #    --socket-path                     /socket/node.socket \
       #    --port                            ${toString relay-port}
       # ''
       #  ];
        ports = [
          (bindPort relay-port)
        ];
        volumes = [
          "${cardanoSocket-relay}:/socket"
          "${cardanoData-relay}:/data"
          "${pkgs.congested.testnet-conf}:/config"
        ];
      };
    };
    node-spo-1 = {
      service = {
        image = "inputoutput/cardano-node:8.1.2";
        networks.default.ipv4_address = "192.168.224.4";
        entrypoint = ''
        sh -c "cardano-node run --config /config/configuration.yaml --topology /config/topology-spo-1.json --database-path /data/db --socket-path /socket/node.socket --port ${toString spo-port} --shelley-kes-key /config/pools/kes1.skey --shelley-operational-certificate /config/pools/opcert1.cert --shelley-vrf-key /config/pools/vrf1.skey --byron-signing-key  /config/byron-gen-command/delegate-keys.000.key --byron-delegation-certificate  /config/byron-gen-command/delegation-cert.000.json & wait"
        '';
        ports = [
          (bindPort spo-port)
        ];
        volumes = [
          "${cardanoSocket-spo}:/socket"
          "${cardanoData-spo}:/data"
          "${pkgs.congested.testnet-conf}:/config"
        ];
      };
    };

    # node-spo-1 = {
    #   image.enableRecommendedContents = true;
    #   service = {
    #     # image = "inputoutput/cardano-node:8.1.2";
    #     # networks.net.aliases = ["node-spo-1.local"];
    #     useHostStore = true;
    #     command = [
    #       "sh"
    #       "-c"
    #       # ''cardano-node run  \
    #       ''${pkgs.congested.cardano-node}/bin/cardano-node run  \
    #       --config                          /config/configuration.yaml \
    #       --topology                        /config/topology-spo-1.json \
    #       --database-path                   /data/db \
    #       --socket-path                     /socket/node.socket \
    #       --port                            ${toString spo-port} \
    #       --shelley-kes-key                 /config/pools/kes1.skey \
    #       --shelley-operational-certificate /config/pools/opcert1.cert \
    #       --shelley-vrf-key                 /config/pools/vrf1.skey \
    #       --byron-signing-key               /config/byron-gen-command/delegate-keys.000.key \
    #       --byron-delegation-certificate    /config/byron-gen-command/delegation-cert.000.json
    #
    #    ''
    #     ];
    #     ports = [
    #       (bindPort spo-port)
    #     ];
    #     volumes = [
    #       "${cardanoSocket-spo}:/socket"
    #       "${cardanoData-spo}:/data"
    #       "${pkgs.congested.testnet-conf}:/config"
    #     ];
    #   };
    # };
  };

}
