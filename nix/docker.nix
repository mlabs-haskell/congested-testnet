{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs, ... }: {


    packages.congested-testnet-image =
      # let
      # cardano-node = pkgs.dockerTools.pullImage {
      #   imageName = "ghcr.io/intersectmbo/cardano-node";
      #   imageDigest = "sha256:9baef8d93eb348a9e28c334d0e1665b6220abd340347505b8989829565ef7193";
      #   sha256 ="sha256-fFkkdyHNhEAYKGRoAHuEwbvQj5rVfEIALouZOBvTB58=";
      # };
      # in
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
        ];
      };
  };
}
