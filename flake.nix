{
  description = "congested-testnet";
  inputs.flake-utils.url = github:numtide/flake-utils;
  inputs.flake-parts.url = github:hercules-ci/flake-parts;
  inputs.arion.url = github:hercules-ci/arion;
  inputs.cardano-node.url = github:input-output-hk/cardano-node/8.1.2;
  inputs.aiken.url = github:aiken-lang/aiken;
  inputs.ctl = {
    type = "github";
    owner = "Plutonomicon";
    repo = "cardano-transaction-lib";
    rev = "b212a58a544d979b5e49dfe5db7f623a2c69e25b";
  };
  inputs.nixpkgs.follows = "ctl/nixpkgs";
  inputs.CHaP = {
    url = github:input-output-hk/cardano-haskell-packages?ref=repo;
    flake = false;
  };
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  inputs.nixos-generators = {
    url = "github:nix-community/nixos-generators";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./nix/shell
        ./nix/ctl
        ./nix/modules
        ./nix/cardano
        ./nix/research
      ];
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };

}
