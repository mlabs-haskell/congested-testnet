{ pkgs, ... }:
let
  cardanoSocket-relay = "node-relay-1-socket";
  cardanoData-relay = "node-relay-1-data";
  cardanoSocket-spo = "node-spo-1-socket";
  cardanoData-spo = "node-spo-1-data";
  testnet-config = "testnet-config";
  spo-port = "3000";
  relay-port = "3000";
  prometheus-port = "12789";
  bindPort = port: "${port}:${port}";
in
{
  project.name = "testnet";

  docker-compose.volumes = {
      "${cardanoSocket-relay}" = {};
      "${cardanoData-relay}" = {};
      "${cardanoSocket-spo}" = {};
      "${cardanoData-spo}" = {};
      "${testnet-config}" = {};
  };


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

    testnet-config = {
      image.enableRecommendedContents = true; 
      service = {
        useHostStore = true;
        command = [
          "sh"
          "-c"
          ''
            ${pkgs.congested.gen-testnet-conf}/bin/gen-testnet-conf  testnet-config 
          ''
        ];
        volumes = [
          "${testnet-config}:/testnet-config"
          ];
      };
    };


    node-relay-1 = {
      service = {
        image = "inputoutput/cardano-node:8.1.2";
        depends_on =["testnet-config"];
        networks.default.aliases = ["node-relay-1.local"];
        entrypoint = ''
         sh -c "cardano-node run --config /config/configuration.yaml --topology /config/topology-relay-1.json --database-path  /data/db --socket-path /socket/node.socket --port ${relay-port}"
         '';
        expose = [relay-port prometheus-port];
        volumes = [
          "${cardanoSocket-relay}:/socket"
          "${cardanoData-relay}:/data"
          "${testnet-config}:/config"
        ];
      };
    };



    node-spo-1 = {
      service = {
        depends_on =["testnet-config"];
        image = "inputoutput/cardano-node:8.1.2";
        networks.default.aliases = ["node-spo-1.local"];
        entrypoint = ''
        sh -c "cardano-node run --config /config/configuration.yaml --topology /config/topology-spo-1.json --database-path /data/db --socket-path /socket/node.socket --port ${spo-port} --shelley-kes-key /config/pools/kes1.skey --shelley-operational-certificate /config/pools/opcert1.cert --shelley-vrf-key /config/pools/vrf1.skey --byron-signing-key  /config/byron-gen-command/delegate-keys.000.key --byron-delegation-certificate  /config/byron-gen-command/delegation-cert.000.json"
        '';
        expose = [spo-port prometheus-port];
        volumes = [
          "${cardanoSocket-spo}:/socket"
          "${cardanoData-spo}:/data"
          "${testnet-config}:/config"
        ];
      };
    };


  };

}
