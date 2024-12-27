{ inputs, self, ... }:
{
  perSystem = { system, inputs', pkgs, self', ... }:
    {
      packages.share-config = pkgs.writeShellApplication {
        name = "share-config";
        runtimeInputs = [
          pkgs.fileshare
          pkgs.coreutils
        ];
        text = ''
           ROOT=$1
           DIR=$2
           PORT=$3
           mkdir -p "$DIR/byron-gen-command"
           cp "$ROOT/byron-gen-command/genesis.json" "$DIR/byron-gen-command/"
           cp "$ROOT/shelley-genesis.json" "$DIR/"
           cp "$ROOT/alonzo-genesis.json" "$DIR/"
           cp "$ROOT/conway-genesis.json" "$DIR/"
           cp "$ROOT/configuration.yaml" "$DIR/"
           cp ${config/topology-spo-2.json} "$DIR/topology.json"
           fileshare -p "$PORT" "$DIR"
        '';
      };

      packages.copy-config = pkgs.writeShellApplication {
        name = "copy-config";
        runtimeInputs = [
          pkgs.fileshare
          pkgs.coreutils
          pkgs.wget
        ];
        text = ''
           URL=$1
           OUT=$2
           cd "$OUT"
           wget -r -nH "$URL"
           touch "$OUT/finish_config" 
        '';
      };
      packages.gen-testnet-conf = pkgs.writeShellApplication {
        name = "gen-testnet-conf";
        runtimeInputs = with pkgs; [
          cardano-node
          cardano-cli
          jq
          coreutils
          gnugrep
          websocat
          curl
          iputils
          bashInteractive
          cacert
          glibcLocales
          iproute
          socat
          utillinux
          dnsutils
          tree
          iproute2
        ];
        text = ''
          ROOT=$1
          BYRON_GENESIS_SPEC_JSON=${../../scripts/byron.genesis.spec.json}
          # if [ -f "$ROOT/finish_config" ]; then
          #   exit 1
          # fi
          ${../../scripts/gen_testnet_conf.sh} "$ROOT" "$BYRON_GENESIS_SPEC_JSON"
          # touch "$ROOT/finish_config" 
        '';
      };

      packages.add-spo-node = pkgs.writeShellApplication {
        name = "add-spo-node";
        runtimeInputs = with pkgs; [
          cardano-node
          cardano-cli
          wget
        ];
        text = ''
          # URL=$1
          DIR=$2
          mkdir -p "$DIR"
          # mkdir -p "$DIR/byron-gen-command"
          # cp "$ROOT/shelley-genesis.json" "$DIR"
          # cp "$ROOT/byron-gen-command/genesis.json" "$DIR/byron-gen-command"
          # cp "$ROOT/conway-genesis.json" "$DIR"
          # cp "$ROOT/alonzo-genesis.json" "$DIR"
          # cp "$ROOT/configuration.yaml" "$DIR"
          cd "$DIR"
          # wget -r -nH "$URL"
          # wget -r --no-parent -nH --cut-dirs=1 -P . "$ROOT"
          # wget -r -np -nH --cut-dirs=1 -P . "$ROOT/byron-genesis-file/"
          # cardano-cli conway address key-gen \
          #   --verification-key-file payment.vkey \
          #   --signing-key-file payment.skey
          #
          # cardano-cli conway stake-address key-gen \
          #   --verification-key-file stake.vkey \
          #   --signing-key-file stake.skey
          #
          # cardano-cli conway stake-address build \
          #   --stake-verification-key-file stake.vkey \
          #   --out-file stake.addr \
          #   --testnet-magic 42
          #
          # cardano-cli conway node key-gen \
          #   --cold-verification-key-file cold.vkey \
          #   --cold-signing-key-file cold.skey \
          #   --operational-certificate-issue-counter cold.counter
          #
          # cardano-cli conway node key-gen-KES \
          #   --verification-key-file kes.vkey \
          #   --signing-key-file kes.skey
          #
          # cardano-cli conway node key-gen-VRF \
          #   --verification-key-file vrf.vkey \
          #   --signing-key-file vrf.skey
          #
          # chmod 400 vrf.key

          # slotsPerKESPeriod=$(cat ../shelley-genesis.json | jq -r '.slotsPerKESPeriod')
          # echo slotsPerKESPeriod: "$slotsPerKESPeriod"
          #
          # slotNo=$(cardano-cli conway query tip --testnet-magic 42 | jq -r '.slot')
          # echo slotNo: "$slotNo"
          #

          cardano-node run --config "configuration.yaml" \
          --database-path "db" \
          --port 3000 \
          --host-addr "0.0.0.0" \
          --socket-path "node.socket" \
          --topology "topology.json"  
          # --shelley-kes-key "$CONFIG/pools-keys/pool1/kes.skey" \
          # --shelley-operational-certificate "$CONFIG/pools-keys/pool1/opcert.cert" \
          # --shelley-vrf-key "$CONFIG/pools-keys/pool1/vrf.skey" \
          # cardano-node run --config "configuration.yaml"  --database-path "db"  --port 3001  --host-addr "0.0.0.0"  --socket-path "node.socket"  --topology "topology.json"  
        '';
      };
};
}

          # cardano-node conway run +RTS -N -A16m -qg -qb -RTS --topology \${TOPOLOGY} --database-path \${DB_PATH} --socket-path \${SOCKET_PATH} --host-addr \${HOSTADDR} --port \${PORT} --config \${CONFIG} --shelley-kes-key \${KES} --shelley-vrf-key \${VRF} --shelley-operational-certificate \${CERT}
