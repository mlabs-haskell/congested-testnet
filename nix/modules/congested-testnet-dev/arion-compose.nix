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
  copy-config = "copy-config";

  spo-port = "3000";
  relay-port = "3000";
  node-prometheus-port = "12789";
  prometheus-port = "9090";
  ogmios-port = "1337";
  kupo-port = "1442";
  faucet-port = "8000";
  cardano-cli-remote-port = "8001";
  share-config-port = "8002";


  config-url = "0.0.0.0:8002/";

  bindPort = port: "${port}:${port}";
  spammer-wallet = name: { "${name}-wallet" = { }; };
  spammer-conf = name: {
    ${name} = {

      image.enableRecommendedContents = true;
      service = {
        useHostStore = true;
        restart = "always";
        command = [
          "sh"
          "-c"
          ''
            ${pkgs.spammer}/bin/spammer wallet 0.0.0.0:8000
          ''
        ];
        volumes = [
          "${name}-wallet:/wallet"
        ];
      };
    };
  };
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
    "${share-config-dir}" = { };
    "${relay-config}" = { };
  }
  // spammer-wallet "spammer-1"
    # // spammer-wallet "spammer-2"
    # // spammer-wallet "spammer-3"
    # // spammer-wallet "spammer-4"
    # // spammer-wallet "spammer-5"
    # // spammer-wallet "spammer-6"
  ;




  services = {


    node-relay-dev-1 = {
      image.enableRecommendedContents = true;
      service = {
        depends_on = [ testnet-config copy-config ];
        useHostStore = true;
        capabilities = { NET_RAW = true; };
        networks.default.aliases = [ "node-relay-dev-1.local" ];
        ports = [ (bindPort relay-port) ];
        defaultExec = [
          "/bin/sh"
          "export PATH=${pkgs.iproute2}/bin:$PATH"
        ];
        entrypoint = ''
          sh -c "
          ${pkgs.relay-node}/bin/relay-node /config /socket /data ${relay-port} topology-relay-dev.json
          "
        '';
        expose = [ relay-port node-prometheus-port ];
        volumes = [
          "${socket-relay}:/socket"
          "${data-relay}:/data"
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




    share-config = {
      image.enableRecommendedContents = true;
      service = {
        restart = "always";
        useHostStore = true;
        networks.default.aliases = [ "share-config.local" ];
        ports = [ (bindPort share-config-port) ];
        volumes = [
          "${testnet-config}:/config"
          "${share-config-dir}:/dir"
        ];
        command = [
          "sh"
          "-c"
          ''
            ${pkgs.share-config}/bin/share-config /config /dir ${share-config-port} 
          ''
        ];
      };
    };

    copy-config = {
      image.enableRecommendedContents = true;
      service = {
        useHostStore = true;
        networks.default.aliases = [ "copy-config.local" ];
        volumes = [
          "${testnet-config}:/config"
        ];
        command = [
          "sh"
          "-c"
          ''
            ${pkgs.copy-config}/bin/copy-config ${config-url} /config  
          ''
        ];
      };
    };

  }
  // spammer-conf "spammer-1"
    # // spammer-conf "spammer-2"
    # // spammer-conf "spammer-3"
    # // spammer-conf "spammer-4"
    # // spammer-conf "spammer-5"
    # // spammer-conf "spammer-6"
  ;

} 
