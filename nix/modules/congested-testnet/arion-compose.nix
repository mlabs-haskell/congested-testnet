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
  faucet-wallet = "faucet-wallet";
  spammer-wallet = "spammer-wallet";

  spo-port = "3000";
  relay-port = "3000";
  node-prometheus-port = "12789";
  prometheus-port = "9090";
  ogmios-port = "1337";
  kupo-port = "1442";
  faucet-port = "8000";

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
    "${faucet-wallet}" = { };
    "${spammer-wallet}" = { };
  };




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

    make-faucet-wallet = {
      image.enableRecommendedContents = true;
      service = {
        depends_on = [ "testnet-config" "node-relay-1" "ogmios" "kupo" ];
        useHostStore = true;
        command = [
          "sh"
          "-c"
          ''
            ${pkgs.congested.make-faucet-wallet}/bin/make-faucet-wallet wallet socket config 
          ''
        ];
        volumes = [
          "${faucet-wallet}:/wallet"
          "${socket-relay}:/socket"
          "${testnet-config}:/config"
        ];
      };
    };

    spammer = {
      image.enableRecommendedContents = true;
      service = {
        depends_on = [ "node-relay-1" "ogmios" "kupo" ];
        useHostStore = true;
        command = [
          "sh"
          "-c"
          ''
            ${pkgs.congested.spammer}/bin/spammer wallet 
          ''
        ];
        volumes = [
          "${spammer-wallet}:/wallet"
          "${faucet-wallet}:/faucet"
        ];
      };
    };

    faucet = {
      image.enableRecommendedContents = true;
      service =
        {
          depends_on = [ "make-faucet-wallet" ];
          networks.default.aliases = [ "faucet.local" ];
          useHostStore = true;
          command = [ "sh" "-c" ''${pkgs.congested.faucet}/bin/faucet'' ];
          ports = [ (bindPort faucet-port) ];
          expose = [ faucet-port ];
          volumes = [
            "${faucet-wallet}:/wallet"
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
        networks.default.aliases = [ "ogmios.local" ];
        useHostStore = true;
        ports = [ (bindPort ogmios-port) ];
        expose = [ ogmios-port ];
        volumes = [
          "${testnet-config}:/config"
          "${socket-relay}:/socket"
        ];
        command = [
          "${pkgs.bash}/bin/sh"
          "-c"
          ''
            ${pkgs.congested.ogmios}/bin/ogmios \
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
        networks.default.aliases = [ "kupo.local" ];
        ports = [ (bindPort kupo-port) ];
        expose = [ kupo-port ];
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
        networks.default.aliases = [ "prometheus.local" ];
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
