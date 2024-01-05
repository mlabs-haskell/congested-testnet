{ inputs, self, ... }:
{
  perSystem = { system, inputs', self', pkgs, ... }:
    let
      runtimeInputs = with pkgs; [
        self'.packages.cardano-node
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
      ];
      relay-node = pkgs.writeShellApplication {
        name = "relay-node";
        inherit runtimeInputs;
        text = ''
          CONFIG=$1
          SOCKET=$2
          DB=$3
          PORT=$4
          TOPOLOGY=$5

          while [ ! -f "$CONFIG/finish_config" ]; do
            sleep 1
            echo "Waiting for testnet configuration to be generated..."
          done
          ln -sf ${pkgs.iana-etc}/etc/protocols /etc/protocols
          ln -sf ${pkgs.iana-etc}/etc/services /etc/services

          cardano-node run --config "$CONFIG/configuration.yaml" \
            --topology "$CONFIG/$TOPOLOGY" \
            --database-path  "$DB/db"  \
            --socket-path "$SOCKET/node.socket"  \
            --port "$PORT" \
            --host-addr "0.0.0.0"
        '';
      };
      spo-node = pkgs.writeShellApplication {
        name = "spo-node";
        inherit runtimeInputs;
        text = ''
          CONFIG=$1
          DB=$2
          PORT=$3
          TOPOLOGY=$4

          while [ ! -f "$CONFIG/finish_config" ]; do
            sleep 1
            echo 'Waiting for testnet configuration to be generated...'
          done

          ln -sf ${pkgs.iana-etc}/etc/protocols /etc/protocols
          ln -sf ${pkgs.iana-etc}/etc/services /etc/services

          cardano-node run --config "$CONFIG/configuration.yaml" \
          --topology "$CONFIG/$TOPOLOGY"  \
          --database-path "$DB/db" \
          --port "$PORT" \
          --shelley-kes-key "$CONFIG/pools/kes1.skey" \
          --shelley-operational-certificate "$CONFIG/pools/opcert1.cert" \
          --shelley-vrf-key "$CONFIG/pools/vrf1.skey" \
          --byron-signing-key  "$CONFIG/byron-gen-command/delegate-keys.000.key" \
          --byron-delegation-certificate  "$CONFIG/byron-gen-command/delegation-cert.000.json" \
          --host-addr "0.0.0.0"
        '';
      };
    in
    {
      packages.relay-node = relay-node;
      packages.spo-node = spo-node;
    };

}
