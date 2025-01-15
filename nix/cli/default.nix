{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs, ... } : {

    packages.congested-testnet-cli = pkgs.writeShellApplication {
        name = "congested-testnet-cli";
        runtimeInputs = with pkgs; [
          fileshare
          prometheus
          cardano-node
          cardano-cli
          kupo
          ogmios
          python311Packages.fire
          spammer
          (python311.withPackages (ps : with ps; [fire]) )
        ];
        text = ''
         # some config files
         export BYRON_GENESIS_SPEC_JSON=${../../scripts/byron.genesis.spec.json}
         export CONFIGURATION_YAML=${../../scripts/configuration.yaml}
         export TOPOLOGY_GENESIS_SPO_JSON=${../../scripts/topology_genesis_spo.json}

         # scripts 
         export GEN_TESTNET_CONF_SH=${../../scripts/gen_testnet_conf.sh}
         export RUN_GENESIS_SPO_SH=${../../scripts/run_genesis_spo.sh}
         export GEN_STAKING_CONF_SH=${../../scripts/gen_staking_conf.sh}
         export RUN_STAKING_NODE_SH=${../../scripts/run_stacking_node.sh}
         export RUN_KUPO_SH=${../../scripts/run_kupo.sh}
         export RUN_OGMIOS_SH=${../../scripts/run_ogmios.sh}
         export RUN_PROMETHEUS_SH=${../../scripts/run_prometheus.sh}
         export CREATE_ADDITIONAL_UTXO_SH=${../../scripts/create_additional_utxo.sh}

         # services: faucet , spammer, metrics vars
         export OGMIOS_PORT="1337"
         export KUPO_PORT="1442"
         export SPAMMER_METRIC_PORT="8001"
         export FAUCET_PORT="8000"
         # export SPAMMER_STATE_FILE="state.json"
         export N_WORKERS=2
         export MEMPOOL_PAUSE_LIMIT=80000
         export MEMPOOL_UNPAUSE_LIMIT=60000

         # cli tool
         python ${./cli.py} "$@"
        '';
      };

    packages.congested-testnet-cli-image = pkgs.dockerTools.buildLayeredImage {
      name = "congested-testnet-cli";
      tag = "latest";
      contents = with pkgs; [
          self'.packages.congested-testnet-cli
          bashInteractive
          jq
          coreutils
          gnugrep
          websocat
          curl
          iputils
          cacert
          glibcLocales
          iproute
          socat
          utillinux
          dnsutils
      ];
    };
    packages.congested-testnet-image =
    # let
    # cardano-node = pkgs.dockerTools.pullImage {
    #   imageName = "ghcr.io/intersectmbo/cardano-node";
    #   imageDigest = "sha256:9baef8d93eb348a9e28c334d0e1665b6220abd340347505b8989829565ef7193";
    #   sha256 ="sha256-fFkkdyHNhEAYKGRoAHuEwbvQj5rVfEIALouZOBvTB58=";
    # };
    # in
    pkgs.dockerTools.buildLayeredImage {
      name = "congested-testnet";
      tag = "latest";
      contents = with pkgs; [
          cardano-node
          jq
          coreutils
          bashInteractive
      ];
    };
    # packages.congested-testnet-cli-image =
    # let
    #   python311 = pkgs.dockerTools.pullImage {
    #       imageName = "python";
    #       imageDigest = ""; # Replace with actual digest
    #   };
    # in
    # pkgs.dockerTools.buildLayeredImage {
    # # packages.congested-testnet-cli-image = pkgs.dockerTools.streamLayeredImage {
    #   name = "congested-testnet-cli";
    #   tag = "latest";
    #   contents = with pkgs; [
    #    spammer
    #   ];
    # };
    # packages.congested-testnet-cli-image = pkgs.dockerTools.buildNixShellImage {
    #   name = "congested-testnet-cli";
    #   tag = "latest";
    #   contents = with pkgs; [
    #       self'.packages.congested-testnet-cli
    #       bashInteractive
    #       jq
    #       coreutils
    #       gnugrep
    #       websocat
    #       curl
    #       iputils
    #       cacert
    #       glibcLocales
    #       iproute
    #       socat
    #       utillinux
    #       dnsutils
    #   ];
    # };
  };
}
