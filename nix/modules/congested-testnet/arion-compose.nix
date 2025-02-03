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
  share-config-dir = "share-config-dir";
  relay-config = "relay-config";
  spo-port = "3000";
  node-prometheus-port = "12789";
  prometheus-port = "9090";
  ogmios-port = "1337";
  kupo-port = "1442";
  faucet-port = "8000";



  bindPort = port: "${port}:${port}";

in
rec {
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
    "${share-config-dir}" = { };
    "${relay-config}" = { };
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
            ${pkgs.gen-testnet-conf}/bin/gen-testnet-conf  testnet-config 
          ''
        ];
        volumes = [
          "${testnet-config}:/testnet-config"
        ];
      };
    };


    generate-additional-utxo-for-ctl = {
      image.enableRecommendedContents = true;
      service =
        {
          networks.default.aliases = [ "faucet.local" ];
          useHostStore = true;
          command = [ "sh" "-c" '''' ];
          entrypoint = ''
             sh -c "
            ${pkgs.generate-additional-utxo-for-ctl}/bin/generate-additional-utxo-for-ctl /wallet /socket /config  
            "
          '';
          volumes = [
            "/tmp/wallet:/wallet"
            "${socket-relay}:/socket"
            "${testnet-config}:/config"
          ];
        };
    };


    node-spo-1 = {
      image.enableRecommendedContents = true;
      service = {
        networks.default.aliases = [ "node-spo-1.local" ];
        useHostStore = true;
        capabilities = { NET_RAW = true; NET_ADMIN = true; };
        defaultExec = [
          "/bin/sh"
          "export PATH=${pkgs.iproute2}/bin:$PATH"
        ];
        entrypoint = ''
           sh -c "
          ${pkgs.spo-node}/bin/spo-node /config /data ${spo-port} topology-spo-1.json /socket
          "
        '';
        expose = [ spo-port node-prometheus-port ];
        volumes = [
          "${socket-relay}:/socket"
          "${data-spo}:/data"
          "${testnet-config}:/config"
        ];
      };
    };


    ogmios = {
      image.enableRecommendedContents = true;
      service = {
        restart = "always";
        networks.default.aliases = [ "ogmios.local" ];
        useHostStore = true;
        ports = [ (bindPort ogmios-port) ];
        expose = [ ogmios-port ];
        volumes = [
          "${testnet-config}:/config"
          "${socket-relay}:/socket"
        ];
        command = [
          "sh"
          "-c"
          ''
            ${pkgs.ogmios-run}/bin/ogmios-run ${ogmios-port} 
          ''
        ];
      };
    };




    kupo = {
      image.enableRecommendedContents = true;
      service = {
        restart = "always";
        networks.default.aliases = [ "kupo.local" ];
        useHostStore = true;
        ports = [ (bindPort kupo-port) ];
        expose = [ kupo-port ];
        volumes = [
          "${testnet-config}:/config"
          "${socket-relay}:/socket"
          "${kupo-db}:/kupo-db"
        ];
        command = [
          "sh"
          "-c"
          ''
            ${pkgs.kupo-run}/bin/kupo-run 
          ''
        ];
      };
    };



    prometheus = {
      image.enableRecommendedContents = true;
      service = {
        restart = "always";
        useHostStore = true;
        networks.default.aliases = [ "prometheus.local" ];
        ports = [ (bindPort prometheus-port) ];
        volumes = [
          "${prometheus-db}:/data"
        ];
        command = [
          "sh"
          "-c"
          ''
            ${pkgs.prometheus-run}/bin/prometheus-run 
          ''
        ];
      };
    };

  };
} 
