{ inputs, self, ... }: {
  imports = [
    ./prometheus.nix
    ./ogmios-kupo.nix
    ./nodes.nix
    ./config.nix
    ./faucet.nix
    ./utils.nix
    ./spammer.nix
  ];
}
