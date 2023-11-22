{
  description = "congested-testnet";
  inputs.flake-utils.url = github:numtide/flake-utils;
  inputs.iohk-nix.url = github:input-output-hk/iohk-nix/v2.2;
  inputs.iogx.url = github:input-output-hk/iogx/be20493284255d15192b3e98ad8b4d51a73b2c8c;
  inputs.cardano-node.url = github:input-output-hk/cardano-node/8.1.2;
  inputs.cardano-node.flake = false;
  inputs.cardano-world.url = github:input-output-hk/cardano-world/f05d6593e2d9b959f5a99461cb10745826efcb64;
  inputs.cardano-world.flake = false;
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


  outputs = { self, nixpkgs, flake-utils, iohk-nix, ctl, cardano-world, cardano-node, ... }@inputs:
    let
      cardano-tag = "8.1.2";
      cardano-version = "8.1.2";
      onchain-outputs = inputs.iogx.lib.mkFlake {
        inherit inputs;
        repoRoot = ./spammer/onchain;
        outputs = import ./spammer/onchain/nix/outputs.nix;
      };
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          overlays = [
            ctl.overlays.purescript
            ctl.overlays.runtime
            ctl.overlays.spago
          ];
          pkgs = import nixpkgs {
            inherit system overlays; config.permittedInsecurePackages =
            [
              "nodejs-14.21.3"
              "openssl-1.1.1w"
            ];
            config.allowBroken = true;
          };

          cardano = pkgs.stdenv.mkDerivation {
            pname = "cardano-binary-source";
            version = cardano-version;

            src = pkgs.fetchurl {
              url = "https://github.com/input-output-hk/cardano-node/releases/download/${cardano-tag}/cardano-node-${cardano-version}-linux.tar.gz";
              sha256 = "sha256-NakRbNfUf1J9NICFOq+HMrfPHurPemdTC8pqf9aeUPo=";
            };

            buildCommand = ''
              mkdir $out
              tar -C $out --strip-components=1 -xf $src
              cd $out
              mkdir $out/bin
              ln -s $out/cardano-cli $out/bin/cardano-cli
              ln -s $out/cardano-node $out/bin/cardano-node
            '';
          };

          inputs'' = inputs // { inherit cardano-tag cardano pkgs; };
          inherit (import ./spammer/nix inputs') psProjectFor;
          ctl-runtime = (psProjectFor pkgs).devShell.buildInputs;
          inputs' = inputs'' // { inherit ctl-runtime; };

          devShell = with pkgs; mkShell {
            buildInputs = [
              pkgs.nixpkgs-fmt
              postgresql_14
              cardano
              dia
              libsodium
              secp256k1
            ] ++ (with pkgs.python310Packages; [ jupyterlab pandas psycopg2 matplotlib tabulate ])
            # ++ onchain-outputs.devShell.${system}.buildInputs
            ++ ctl-runtime;
            shellHook = ''
            '';
          };

        in
        {

          devShells.default = devShell;
          packages = onchain-outputs.packages.${system} //
            # generate config for cardano testnet
            (import ./config inputs') //
            # run testnet with docker compose
            (import ./cluster (
              inputs' // { gen-testnet-config = self.packages.${system}.config; }
            )) //
            (import ./spammer/nix inputs') //
            rec {
              ctl-node = (psProjectFor pkgs).buildPursProject {
                main = "Spammer.Main";
                entrypoint = "index.js";
              };
              test = import ./test inputs';
              research = import ./research inputs';
            };

          apps = {
            purs-docs = (psProjectFor pkgs).launchSearchablePursDocs { };
          };

        }
      );
}
