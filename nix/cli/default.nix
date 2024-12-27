{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs, ... } : {
    # packages.congested-testnet-cli = pkgs.dockerTools.buildImage {
    # name = "congested-testnet-cli";
    # config.Cmd = "${self'.packages.gen-testnet-conf}/bin/gen-testnet-conf"; 
    # };

    packages.congested-testnet-cli = pkgs.writeShellApplication {
        name = "congested-testnet-cli";
        runtimeInputs = with pkgs; [
          fileshare
          cardano-node
          cardano-cli
          python311Packages.fire
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

         python ${./cli.py} "$@"
        '';
      };
  };
}
          # ROOT="congested-testnet-$(openssl rand -hex 4)"
          # export ROOT
          # mkdir -p "$ROOT"
          # echo "testnet configs in $ROOT"
          # gen-testnet-conf "$ROOT"
          # cardano-node run --config "$ROOT/configuration.yaml" \
          #     --database-path "$ROOT/db" \
          #     --port 3000 \
          #     --shelley-kes-key "$ROOT/pools-keys/pool1/kes.skey" \
          #     --shelley-operational-certificate "$ROOT/pools-keys/pool1/opcert.cert" \
          #     --shelley-vrf-key "$ROOT/pools-keys/pool1/vrf.skey" \
          #     --byron-signing-key  "$ROOT/byron-gen-command/delegate-keys.000.key" \
          #     --byron-delegation-certificate  "$ROOT/byron-gen-command/delegation-cert.000.json" \
          #     --host-addr "0.0.0.0" \
          #     --socket-path "$ROOT/node.socket" \
          #     --topology ${../containers/config/topology-spo-1.json}  
