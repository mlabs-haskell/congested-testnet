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

    packages.congested-testnet-image-example =
      pkgs.dockerTools.buildLayeredImage {
        name = "cgnet-example";
        tag = "latest";
        contents = with pkgs; [
          cardano-node
          bashInteractive
          pkgs.nodejs
        ];
      };
  };
}
