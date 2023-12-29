{
  description = "congested-testnet";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.arion.url = "github:hercules-ci/arion";
  inputs.cardano-node.url = "github:input-output-hk/cardano-node/8.1.2";
  inputs.aiken.url = "github:aiken-lang/aiken";
  inputs.ctl = {
    type = "github";
    owner = "Plutonomicon";
    repo = "cardano-transaction-lib";
    rev = "b212a58a544d979b5e49dfe5db7f623a2c69e25b";
  };
  inputs.nixpkgs.follows = "ctl/nixpkgs";
  inputs.CHaP = {
    url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
    flake = false;
  };
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

  # outputs = { self, nixpkgs, flake-utils, iohk-nix, ctl, cardano-world, cardano-node, ... }@inputs:
  #   let
  #     cardano-tag = "8.1.2";
  #     cardano-version = "8.1.2";
  #   in
  #   flake-utils.lib.eachDefaultSystem
  #     (system:
  #       let
  #         overlays = [
  #           ctl.overlays.purescript
  #           ctl.overlays.runtime
  #           ctl.overlays.spago
  #         ];
  #         pkgs = import nixpkgs {
  #           inherit system overlays; config.permittedInsecurePackages =
  #           [
  #             "nodejs-14.21.3"
  #             "openssl-1.1.1w"
  #           ];
  #           config.allowBroken = true;
  #         };
  #         pkgs-unstable = import inputs.nixpkgs-unstable { inherit system; config.allowBroken = true; };
  #
  #
  #         cardano = pkgs.stdenv.mkDerivation {
  #           pname = "cardano-binary-source";
  #           version = cardano-version;
  #
  #           src = pkgs.fetchurl {
  #             url = "https://github.com/input-output-hk/cardano-node/releases/download/${cardano-tag}/cardano-node-${cardano-version}-linux.tar.gz";
  #             sha256 = "sha256-NakRbNfUf1J9NICFOq+HMrfPHurPemdTC8pqf9aeUPo=";
  #           };
  #
  #           buildCommand = ''
  #             mkdir $out
  #             tar -C $out --strip-components=1 -xf $src
  #             cd $out
  #             mkdir $out/bin
  #             ln -s $out/cardano-cli $out/bin/cardano-cli
  #             ln -s $out/cardano-node $out/bin/cardano-node
  #           '';
  #         };
  #
  #         inputs'' = inputs // { inherit cardano-tag cardano pkgs system; };
  #         inherit (import ./spammer/nix inputs'') spammer;
  #         psProjectFor = spammer.psProjectFor;
  #         ctl-runtime = (psProjectFor pkgs).devShell.buildInputs;
  #         inputs' = inputs'' // { inherit ctl-runtime spammer; };
  #
  #         devShell = with pkgs; mkShell {
  #           buildInputs = [
  #             pkgs.nixpkgs-fmt
  #             postgresql_14
  #             cardano
  #             dia
  #             libsodium
  #             secp256k1
  #             yq
  #           ] ++ (with pkgs-unstable.python310Packages; [ pycardano cbor jupyterlab pandas psycopg2 matplotlib tabulate ])
  #           # ++ onchain-outputs.devShell.${system}.buildInputs
  #           ++ [ inputs.aiken.outputs.packages.${system}.aiken ]
  #           ++ ctl-runtime;
  #           shellHook = ''
  #           '';
  #         };
  #
  #       in
  #       {
  #         devShells.default = devShell;
  #         packages = 
  #           # generate config for cardano testnet
  #           (import ./config inputs') //
  #           # run testnet with docker compose
  #           (import ./cluster (
  #             inputs' // { gen-testnet-config = self.packages.${system}.config; }
  #           )) //
  #           (import ./spammer/nix inputs') //
  #           rec {
  #             ctl-node = (psProjectFor pkgs).buildPursProject {
  #               main = "Spammer.Main";
  #               entrypoint = "index.js";
  #             };
  #             test = import ./test inputs';
  #             research = import ./research inputs';
  #           };
  #
  #         apps = {
  #           purs-docs = (psProjectFor pkgs).launchSearchablePursDocs { };
  #         };
  #
  #       }
  #     );
}
