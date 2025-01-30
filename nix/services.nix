{ lib, pkgs, modulesPath, ... }:
let
  root = "/var/congested-testnet";
  prometheus-data = "/var/prometheus-data";
  share-config = "/var/share-config";
in
{

  systemd.services.cardano-node =
    let
      make-config-and-run-spo = pkgs.writeShellApplication {
        name = "make-config-and-run-spo";
        runtimeInputs = with pkgs; [
          cardano-node
          jq
        ];
        text = ''
          export  BYRON_GENESIS_SPEC_JSON=${../scripts/byron.genesis.spec.json}
          export  CONFIGURATION_YAML=${../scripts/configuration.yaml}
          export  TOPOLOGY_GENESIS_SPO_JSON=${../scripts/topology_genesis_spo.json}
          export  PORT=3000
          export  SHARE=${share-config}
          export  ROOT=${root}
          rm -rf "$ROOT"
          rm -rf "$SHARE"
          mkdir -p "$ROOT"
          mkdir -p "$SHARE"
          ${../scripts/gen_testnet_conf.sh} 
          ${../scripts/run_genesis_spo.sh} 
        '';
      };
    in
    {
      description = "run cardano node";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = ''
          ${make-config-and-run-spo}/bin/make-config-and-run-spo 
        '';
        Restart = "on-failure";
        RestartSec = 5;
      };
      reloadIfChanged = true;
    };


  systemd.services.ogmios =
    let ogmios = pkgs.writeShellApplication {
        name = "ogmios";
        runtimeInputs = [
          pkgs.ogmios
        ];
        text = ''
          export ROOT=${root}
          ${../scripts/run_ogmios.sh} 
        '';
      };
    in
    {
      description = "ogmios";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = ''
          ${ogmios}/bin/ogmios
        '';
        Restart = "on-failure";
        RestartSec = 5;
      };
      reloadIfChanged = true;
    };
  
    systemd.services.restart-network = {
      description = "restart network";
      serviceConfig = {
        ExecStart = ''
          systemctl restart cardano-node.service kupo.service ogmios.service spammer-and-faucet.service share-config.service
        '';
      };
    };
 
    systemd.timers.restart-schedule = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnBootSec = "1s";
        OnUnitActiveSec = "2h";
        Unit = "restart-network.service";
      };
    };
  
    systemd.services.kupo =
      let
        kupo = pkgs.writeShellApplication {
          name = "kupo";
          runtimeInputs = [
            pkgs.kupo
          ];
          text = ''
            ROOT=${root}
            ${../scripts/run_kupo.sh} 
          '';
        };
    in
    {
      description = "kupo";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = ''
          ${kupo}/bin/kupo
        '';
        Restart = "on-failure";
        RestartSec = 5;
      };
      reloadIfChanged = true;
    };
  
  
  # systemd.services.spammer-and-faucet =
  #   let
  #     helper = pkgs.writeShellApplication {
  #       runtimeInputs = [
  #         pkgs.spammer
  #       ];
  #       name = "spammer";
  #       text = ''
  #         ROOT=${root}
  #         if [ ! -f "$ROOT/faucet_wallet_exist" ]; then
  #           ${pkgs.generate-additional-utxo-for-ctl}/bin/generate-additional-utxo-for-ctl $ROOT $ROOT $ROOT
  #         fi
  #         export ogmiosUrl="0.0.0.0" 
  #         export kupoUrl="0.0.0.0" 
  #         export walletPath="$ROOT/wallet.skey"
  #         spammer
  #         
  #       '';
  #     };
  #   in
  #   {
  #     description = "spammer";
  #     wantedBy = [ "multi-user.target" ];
  #     serviceConfig = {
  #       ExecStart = ''${helper}/bin/spammer'';
  #       Restart = "on-failure";
  #       RestartSec = 5;
  #     };
  #     reloadIfChanged = true;
  #   };
  #
  # systemd.services.prometheus = {
  #   description = "prometheus";
  #   wantedBy = [ "multi-user.target" ];
  #   serviceConfig = {
  #     ExecStart = ''${pkgs.prometheus}/bin/prometheus --config.file=${../../containers/prometheus.yaml} --storage.tsdb.path=${prometheus-data} --storage.tsdb.retention.time=4320h'';
  #     Restart = "on-failure";
  #     RestartSec = 5;
  #   };
  # };
  #
  # systemd.services.share-config =
  #   {
  #     description = "share-config";
  #     wantedBy = [ "multi-user.target" ];
  #     serviceConfig = {
  #       ExecStart = ''${pkgs.share-config}/bin/share-config ${root} ${share-config} 5000'';
  #       Restart = "on-failure";
  #       RestartSec = 5;
  #     };
  #     reloadIfChanged = true;
  #   };

}


