{ inputs, self, ... }:
{
  imports = [
    ./overlays.nix
    ./cardano-node.nix
    ./spammer.nix
    ./docker.nix
    ./generate-scripts.nix
    ./ogmios-kupo.nix
    ./research.nix
    # ./services.nix
    ./shell.nix
    ./tests.nix
    ./utils.nix
  ];
}
