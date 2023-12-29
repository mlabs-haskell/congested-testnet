{ pkgs, ... }:
let
  socket-relay = "node-relay-1-socket";
  data-relay = "node-relay-1-data";
  socket-spo = "node-spo-1-socket";
  data-spo = "node-spo-1-data";
  data-kupo = "data-kupo";
  testnet-config = "testnet-config";
  kupo-db = "kupo-db";
  prometheus-db = "prometheus";

  spo-port = "3000";
  relay-port = "3000";
  node-prometheus-port = "12789";
  prometheus-port = "9090";
  ogmios-port = "1337";
  kupo-port = "1442";

  bindPort = port: "${port}:${port}";
in
{
  project.name = "testnet";

  docker-compose.volumes = {
    "${socket-relay}" = { };
    "${data-relay}" = { };
    "${socket-spo}" = { };
    "${data-spo}" = { };
    "${testnet-config}" = { };
    "${kupo-db}" = { };
    "${prometheus-db}" = { };
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
            ${pkgs.gen-testnet-conf}/bin/gen-testnet-conf  testnet-config 
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
        depends_on = [ "testnet-config" ];
        networks.default.aliases = [ "node-relay-1.local" ];
        entrypoint = ''
          sh -c "cardano-node run --config /config/configuration.yaml --topology /config/topology-relay-1.json --database-path  /data/db --socket-path /socket/node.socket --port ${relay-port}"
        '';
        expose = [ relay-port node-prometheus-port ];
        volumes = [
          "${socket-relay}:/socket"
          "${data-relay}:/data"
          "${testnet-config}:/config"
        ];
      };
    };




    node-spo-1 = {
      service = {
        depends_on = [ "testnet-config" ];
        image = "inputoutput/cardano-node:8.1.2";
        networks.default.aliases = [ "node-spo-1.local" ];
        entrypoint = ''
          sh -c "cardano-node run --config /config/configuration.yaml --topology /config/topology-spo-1.json --database-path /data/db --socket-path /socket/node.socket --port ${spo-port} --shelley-kes-key /config/pools/kes1.skey --shelley-operational-certificate /config/pools/opcert1.cert --shelley-vrf-key /config/pools/vrf1.skey --byron-signing-key  /config/byron-gen-command/delegate-keys.000.key --byron-delegation-certificate  /config/byron-gen-command/delegation-cert.000.json"
        '';
        expose = [ spo-port node-prometheus-port ];
        volumes = [
          "${socket-spo}:/socket"
          "${data-spo}:/data"
          "${testnet-config}:/config"
        ];
      };
    };




    ogmios = {
      service = {
        depends_on = [ "testnet-config" ];
        useHostStore = true;
        ports = [ (bindPort ogmios-port) ];
        volumes = [
          "${testnet-config}:/config"
          "${socket-relay}:/socket"
        ];
        command = [
          "${pkgs.bash}/bin/sh"
          "-c"
          ''
            ${pkgs.ogmios}/bin/ogmios \
              --host ogmios \
              --port ${ogmios-port} \
              --node-socket /socket/node.socket \
              --node-config /config/configuration.yaml \
              --include-transaction-cbor
          ''
        ];
      };
    };




    kupo = {
      service = {
        image = "cardanosolutions/kupo:v2.2.0";
        ports = [ (bindPort kupo-port) ];
        volumes = [
          "${testnet-config}:/config"
          "${socket-relay}:/socket"
          "${kupo-db}:/kupo-db"
        ];
        command = [
          "--node-config"
          "/config/configuration.yaml"
          "--node-socket"
          "/socket/node.socket"
          "--since"
          "origin"
          "--match"
          "*/*"
          "--host"
          "0.0.0.0"
          "--workdir"
          "kupo-db"
          "--prune-utxo"
          "--defer-db-indexes"
        ];
      };
    };



    prometheus = {
      service = {
        useHostStore = true;
        image = "prom/prometheus:v2.43.1";
        ports = [ (bindPort prometheus-port) ];
        volumes = [
          "${prometheus-db}:/prometheus"
        ];
        command = [
          ''--config.file=${./prometheus.yaml}''
        ];
      };
    };




  };

}
