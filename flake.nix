{
  description = "congested-testnet";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.arion.url = "github:hercules-ci/arion/f295eabd25b7c894ab405be784e2a010f83fde55";
  inputs.aiken.url = "github:aiken-lang/aiken/7b452c21f01e5342b2c210e0a32f024c58ea2693";
  inputs.ctl = {
    type = "github";
    owner = "Plutonomicon";
    repo = "cardano-transaction-lib";
    rev = "b212a58a544d979b5e49dfe5db7f623a2c69e25b";
  };
  inputs.nixpkgs.follows = "ctl/nixpkgs";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./nix/shell
        ./nix/ctl
        ./nix/modules
        ./nix/containers
        ./nix/cardano
        ./nix/research
      ];
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      flake.herculesCI.ciSystems = [ "x86_64-linux" ];
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
