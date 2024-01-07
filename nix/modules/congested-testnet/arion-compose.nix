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

  spo-port = "3000";
  relay-port = "3000";
  node-prometheus-port = "12789";
  prometheus-port = "9090";
  ogmios-port = "1337";
  kupo-port = "1442";
  faucet-port = "8000";

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
            ${pkgs.spammer}/bin/spammer wallet 
          ''
        ];
        volumes = [
          "${name}-wallet:/wallet"
          "${faucet-wallet}:/faucet"
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
  } 
  // spammer-wallet "spammer-1" 
  // spammer-wallet "spammer-2"
  // spammer-wallet "spammer-3"
  // spammer-wallet "spammer-4"
  // spammer-wallet "spammer-5"
  // spammer-wallet "spammer-6"
  ;




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

    # ping-relay-spo = {
    #   image.enableRecommendedContents = true;
    #   service = {
    #     useHostStore = true;
    #     capabilities = { NET_ADMIN = true; };
    #     command = [
    #       "sh"
    #       "-c"
    #       ''
    #         ${pkgs.ping-relay-spo}/bin/ping-relay-spo 
    #       ''
    #     ];
    #   };
    # };


    faucet = {
      image.enableRecommendedContents = true;
      service =
        {
          networks.default.aliases = [ "faucet.local" ];
          useHostStore = true;
          command = [ "sh" "-c" ''${pkgs.faucet}/bin/faucet'' ];
          ports = [ (bindPort faucet-port) ];
          expose = [ faucet-port ];
          volumes = [
            "${faucet-wallet}:/wallet"
            "${socket-relay}:/socket"
            "${testnet-config}:/config"
          ];
        };
    };



    node-relay-1 = {
      image.enableRecommendedContents = true;
      service = {
        useHostStore = true;
        capabilities = { NET_RAW = true; NET_ADMIN = true;};
        networks.default.aliases = [ "node-relay-1.local" ];
        defaultExec = [
         "/bin/sh"
         "export PATH=${pkgs.iproute2}/bin:$PATH"
        ];
        entrypoint = ''
          sh -c "
          ${pkgs.relay-node}/bin/relay-node /config /socket /data ${relay-port} topology-relay-1.json
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


    node-spo-1 = {
      image.enableRecommendedContents = true;
      service = {
        networks.default.aliases = [ "node-spo-1.local" ];
        useHostStore = true;
        capabilities = { NET_RAW = true; NET_ADMIN = true;};
        defaultExec = [
         "/bin/sh"
         "export PATH=${pkgs.iproute2}/bin:$PATH"
        ];
        entrypoint = ''
           sh -c "
          ${pkgs.spo-node}/bin/spo-node /config /data ${spo-port} topology-spo-1.json
          "
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
      image.enableRecommendedContents = true;
      service = {
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


  } 
  // spammer-conf "spammer-1"
  // spammer-conf "spammer-2"
  // spammer-conf "spammer-3"
  // spammer-conf "spammer-4"
  // spammer-conf "spammer-5"
  // spammer-conf "spammer-6"
  ;

} 
