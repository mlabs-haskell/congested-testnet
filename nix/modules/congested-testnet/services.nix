{ lib, pkgs, modulesPath, ... }:
let
 root = "/var/congested-testnet";
 prometheus-data = "/var/prometheus-data";
in
{

  systemd.services.cardano-node = let 
      make-config-and-run-spo = pkgs.writeShellApplication {
        name = "make-config-and-run-spo";
        text = ''
          ROOT=${root}
          rm -rf "$ROOT"
          mkdir -p "$ROOT"
          ${pkgs.gen-testnet-conf}/bin/gen-testnet-conf $ROOT  
          ${pkgs.cardano-node}/bin/cardano-node run --config "$ROOT/configuration.yaml" \
              --database-path "$ROOT/db" \
              --port 3000 \
              --shelley-kes-key "$ROOT/pools-keys/pool1/kes.skey" \
              --shelley-operational-certificate "$ROOT/pools-keys/pool1/opcert.cert" \
              --shelley-vrf-key "$ROOT/pools-keys/pool1/vrf.skey" \
              --byron-signing-key  "$ROOT/byron-gen-command/delegate-keys.000.key" \
              --byron-delegation-certificate  "$ROOT/byron-gen-command/delegation-cert.000.json" \
              --host-addr "0.0.0.0" \
              --socket-path "$ROOT/node.socket" \
              --topology ${../../containers/config/topology-spo-1.json}  
        '';
      };
  in
  {
    description = "run cardano node"; 
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      ExecStart = ''
        ${make-config-and-run-spo}/bin/make-config-and-run-spo 
      '';
      Restart = "on-failure";
      RestartSec = 5;
    };
  };


  systemd.services.ogmios = let 
      ogmios = pkgs.writeShellApplication {
        name = "ogmios";
        runtimeInputs = [
            pkgs.ogmios
        ];
        text = ''
          ROOT=${root}
          ogmios \
              --host 0.0.0.0 \
              --port 1337 \
              --node-socket $ROOT/node.socket \
              --node-config $ROOT/configuration.yaml \
              --include-transaction-cbor
        '';
      };
  in
  {
    description = "ogmios"; 
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      ExecStart = ''
       ${ogmios}/bin/ogmios
      '';
      Restart = "on-failure";
      RestartSec = 5;
    };
  };

  systemd.services.kupo = let 
      kupo = pkgs.writeShellApplication {
        name = "kupo";
        runtimeInputs = [
            pkgs.kupo
        ];
        text = ''
          ROOT=${root}
          kupo \
          --node-config $ROOT/configuration.yaml \
          --node-socket $ROOT/node.socket \
          --since origin \
          --match "*/*" \
          --host 0.0.0.0 \
          --workdir $ROOT \
          --prune-utxo \
          --defer-db-indexes 
        '';
      };
  in
  {
    description = "kupo"; 
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      ExecStart = ''
       ${kupo}/bin/kupo
      '';
      Restart = "on-failure";
      RestartSec = 5;
    };
  };


  systemd.services.spammer-and-faucet = let 
      helper = pkgs.writeShellApplication {
        runtimeInputs = [
            pkgs.spammer
        ];
        name = "spammer";
        text = ''
          ROOT=${root}
          if [ ! -f "$ROOT/faucet_wallet_exist" ]; then
            ${pkgs.generate-additional-utxo-for-ctl}/bin/generate-additional-utxo-for-ctl $ROOT $ROOT $ROOT
          fi
          export ogmiosUrl="0.0.0.0" 
          export kupoUrl="0.0.0.0" 
          export walletPath="$ROOT/wallet.skey"
          spammer
          
        '';
      };
  in
  {
    description = "spammer"; 
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      ExecStart = ''${helper}/bin/spammer'';
      Restart = "on-failure";
      RestartSec = 5;
    };
  };

  systemd.services.prometheus = {
    description = "prometheus"; 
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      ExecStart = ''${pkgs.prometheus}/bin/prometheus --config.file=${../../containers/prometheus.yaml} --storage.tsdb.path=${prometheus-data} --storage.tsdb.retention.time=4320h'';
      Restart = "on-failure";
      RestartSec = 5;
    };
  };

  environment.systemPackages = [
    pkgs.htop
    pkgs.spammer
  ];
}


