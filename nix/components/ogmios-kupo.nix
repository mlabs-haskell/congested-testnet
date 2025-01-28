{ inputs, self, ... }:
{
  perSystem = { system, inputs', self', pkgs, ... }:
    let
      runtimeInputs = [
        pkgs.coreutils
        pkgs.ogmios
        pkgs.kupo
      ];
    in
    {
      packages.ogmios = pkgs.stdenv.mkDerivation {
        pname = "ogmios";
        version = "6.11.0";

        src = pkgs.fetchurl {
          # url = "https://github.com/IntersectMBO/cardano-node/releases/download/10.1.1/cardano-node-10.1.1-linux.tar.gz";
          # sha256 = "sha256-XH9T84KKZzpyGIUOV+vWGLC0rQZzZOQTHhO1qXoYgnI=";
          url = "https://github.com/CardanoSolutions/ogmios/releases/download/v6.11.0/ogmios-v6.11.0-aarch64-linux.zip";
          sha256 = "";
        };

        buildCommand = ''
          mkdir $out
          tar -C $out --strip-components=1 -xf $src
        '';
      };

      packages.ogmios-run = pkgs.writeShellApplication {
        name = "ogmios-run";
        inherit runtimeInputs;
        text = ''
          while [ ! -f "/config/finish_config" ]; do
            sleep 1
            echo "Waiting for testnet configuration to be generated..."
          done
          ln -sf ${pkgs.iana-etc}/etc/protocols /etc/protocols
          ln -sf ${pkgs.iana-etc}/etc/services /etc/services

          ogmios \
              --host ogmios \
              --port "$1" \
              --node-socket /socket/node.socket \
              --node-config /config/configuration.yaml \
              --include-transaction-cbor

        '';
      };
      packages.kupo-run = pkgs.writeShellApplication {
        name = "kupo-run";
        inherit runtimeInputs;
        text = ''
          while [ ! -f "/config/finish_config" ]; do
            sleep 1
            echo 'Waiting for testnet configuration to be generated...'
          done
          ln -sf ${pkgs.iana-etc}/etc/protocols /etc/protocols
          ln -sf ${pkgs.iana-etc}/etc/services /etc/services

          kupo \
          --node-config /config/configuration.yaml \
          --node-socket /socket/node.socket \
          --since origin \
          --match "*/*" \
          --host 0.0.0.0 \
          --workdir kupo-db \
          --prune-utxo \
          --defer-db-indexes 

        '';
      };
    };
}
