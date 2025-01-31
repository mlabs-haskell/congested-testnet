{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs, ... }: {


    packages.congested-testnet-image =
      pkgs.dockerTools.buildLayeredImage {
        name = "congested-testnet";
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
  };
}
