{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs, ... }: {


    packages.congested-testnet-image =
      pkgs.dockerTools.buildLayeredImage {
        name = "cgnet";
        tag = "latest";
        contents = with pkgs; [
          cardano-node
          jq
          coreutils
          bashInteractive
          ogmios
          kupo
          spammer
          prometheus
          # fileshare
        ];
      };

    packages.congested-testnet-relay-image =
      pkgs.dockerTools.buildLayeredImage {
        name = "cgnet-relay";
        tag = "latest";
        contents = with pkgs; [
          cardano-node
          wget
          coreutils
          bashInteractive
        ];
      };

  };
}
