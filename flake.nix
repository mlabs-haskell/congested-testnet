{
  description = "congested-testnet";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.arion.url = "github:hercules-ci/arion/f295eabd25b7c894ab405be784e2a010f83fde55";
  inputs.aiken.url = "github:aiken-lang/aiken/7b452c21f01e5342b2c210e0a32f024c58ea2693";
  inputs.easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix";
  inputs.ctl.url = "github:Plutonomicon/cardano-transaction-lib/v9.3.1";
  inputs.nixpkgs.follows = "ctl/nixpkgs";
  inputs.spo-anywhere.url = "github:mlabs-haskell/spo-anywhere";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };

  inputs.crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./nix/overlays.nix
        ./nix/ctl
        ./nix/shell
        ./nix/modules
        ./nix/containers
        ./nix/cardano
        # ./nix/research
        ./nix/docker
        ./nix/tests
        ./examples/get-tada.nix
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
