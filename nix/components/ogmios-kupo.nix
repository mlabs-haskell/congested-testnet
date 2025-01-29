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
        buildInputs = [pkgs.unzip];

        src = pkgs.fetchurl {
          url = "https://github.com/CardanoSolutions/ogmios/releases/download/v6.11.0/ogmios-v6.11.0-x86_64-linux.zip";
          sha256 = "sha256-zrm46h/CThI8ZVLi4XBeo4q2a61zQD0ZQpRUiVO9798=";
        };

        buildCommand = ''
          mkdir $out
          unzip $src -d $out
          chmod -R +x $out/bin
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
