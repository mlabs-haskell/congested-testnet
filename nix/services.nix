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
          bashInteractive
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

  systemd.services.post_run_genesis_node =
    let
      helper = pkgs.writeShellApplication {
        name = "post_run_genesis_node";
        runtimeInputs = [
        pkgs.cardano-node
        pkgs.bashInteractive
        pkgs.jq
        ];
        text = ''
          export ROOT=${root}
          export SHARE=${share-config}
          ${../scripts/post_run_genesis_node.sh} 
        '';
      };
    in
    {
      description = "post_run_genesis_node";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = ''
          ${helper}/bin/post_run_genesis_node
        '';
        Restart = "on-failure";
        RestartSec = 5;
      };
      reloadIfChanged = true;
    };

  systemd.services.ogmios =
    let
      ogmios = pkgs.writeShellApplication {
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
          export ROOT=${root}
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

  systemd.services.prometheus =
    let
      prometheus = pkgs.writeShellApplication {
        name = "prometheus";
        runtimeInputs = [
          pkgs.prometheus
          pkgs.bashInteractive
        ];
        text = ''
          export DATA=${prometheus-data}
          export CARDANO_NODE_METRICS_URL=0.0.0.0:12789
          export SPAMMER_METRICS_URL=0.0.0.0:8001
          ${../scripts/run_prometheus.sh} 
        '';
      };
    in
    {
      description = "prometheus";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = ''
          ${prometheus}/bin/prometheus
        '';
        Restart = "on-failure";
        RestartSec = 5;
      };
      reloadIfChanged = true;
    };
    
  systemd.services.spammer =
    let
      spammer = pkgs.writeShellApplication {
        name = "spammer";
        runtimeInputs = [
          pkgs.spammer
          pkgs.bashInteractive
        ];
        text = ''
          export  WALLET_SKEY_PATH=${root}/wallet.skey
          export  SPAMMER_STATE_FILE=${root}/state.json
          export  CARDANO_NODE_METRICS_URL=0.0.0.0:12789
          export  SPAMMER_METRIC_PORT=8001
          export  FAUCET_PORT=8000
          export  N_WORKERS=2
          export  OGMIOS_URL=0.0.0.0
          export  KUPO_URL=0.0.0.0
          export  MEMPOOL_PAUSE_LIMIT=80000
          # off if you don't need spammer or faucet
          export  SPAMMER_ON=true
          export  FAUCET_ON=true
          sleep 8
          spammer
        '';
      };
    in
    {
      description = "spammer";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = ''
          ${spammer}/bin/spammer
        '';
        Restart = "on-failure";
        RestartSec = 5;
      };
      reloadIfChanged = true;
    };


  systemd.services.share-config =
    {
      description = "share-config";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = ''${pkgs.python311}/bin/python -m http.server 5000 --directory ${share-config}'';
        Restart = "on-failure";
        RestartSec = 5;
      };
      reloadIfChanged = true;
    };
  
}


