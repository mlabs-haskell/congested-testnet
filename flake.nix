{
  description = "congested-testnet";
  inputs.flake-utils.url = github:numtide/flake-utils;
  inputs.iohk-nix.url = github:input-output-hk/iohk-nix/v2.2;
  inputs.iogx.url = github:input-output-hk/iogx/be20493284255d15192b3e98ad8b4d51a73b2c8c;
  inputs.cardano-node.url = github:input-output-hk/cardano-node/1.35.6;
  inputs.cardano-node.flake = false;
  inputs.cardano-world.url = github:input-output-hk/cardano-world/f05d6593e2d9b959f5a99461cb10745826efcb64;
  inputs.cardano-world.flake = false;
  inputs.plutus.url = "github:input-output-hk/plutus/a49a91f467930868a3b6b08f194d94ae3f0e086e";
  # inputs.plutus.url = "github:input-output-hk/plutus/1.1.0.0";
  inputs.ctl = {
    type = "github";
    owner = "Plutonomicon";
    repo = "cardano-transaction-lib";
    rev = "605931759ff35bdd71bb4d933071aced9fb57870";
  };

  inputs.nixpkgs.follows = "ctl/nixpkgs";
  inputs.CHaP.follows = "ctl/CHaP";

  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };


  outputs = { self, nixpkgs, flake-utils, iohk-nix, ctl, cardano-world, cardano-node, ... }@inputs:
    let
      cardano-tag = "1.35.6";
      onchain-outputs = inputs.iogx.lib.mkFlake {
        inherit inputs;
        repoRoot = ./spamer/onchain;
        outputs = import ./spamer/onchain/nix/outputs.nix;
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
            pname = "cardano-static";
            version = cardano-tag;

            src = pkgs.fetchurl {
              url = "https://update-cardano-mainnet.iohk.io/cardano-node-releases/cardano-node-${cardano-tag}-linux.tar.gz";
              sha256 = "sha256-R4+5qbHyFLIvwHb5x9uTxLDdOPFwBADrjKRP6eTnoBE=";
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
          psProjectFor = pkgs:
            pkgs.purescriptProject rec {
              inherit pkgs;
              projectName = "ctl-node";
              packageJson = ./spamer/basic-spamer-ctl/package.json;
              packageLock = ./spamer/basic-spamer-ctl/package-lock.json;
              src = builtins.path {
                path = ./spamer/basic-spamer-ctl;
                name = "${projectName}-src";
                # Adjust the `filter` as necessary
                filter = path: ftype: !(pkgs.lib.hasSuffix ".md" path);
              };
              shell = {
                withRuntime = true;
                packageLockOnly = true;
                packages = with pkgs; [
                  fd
                  nodePackages.eslint
                  nodePackages.prettier
                ];
              };
            };

          devShell = with pkgs; mkShell {
            buildInputs = [
              pkgs.nixpkgs-fmt
              postgresql_14
              cardano
              dia
            ] ++ (with pkgs.python310Packages; [ jupyterlab pandas psycopg2 matplotlib tabulate ])
            ++ onchain-outputs.devShell.${system}.buildInputs
            ++ (psProjectFor pkgs).devShell.buildInputs;
            shellHook = ''
            '';
          };

          root = ./.;

        in
        {
          devShells.default = devShell;
          packages = onchain-outputs.packages.${system} //
            (import ./config { inherit pkgs iohk-nix cardano-world system cardano cardano-node; }) //
            rec {
              # generate config files for testnet
              ctl-node = (psProjectFor pkgs).buildPursProject {
                main = "Spamer.Main";
                entrypoint = "index.js";
              };
              # run testnet with docker compose
              runnet = import ./cluster { inherit pkgs cardano; tags = { inherit cardano-tag; }; };
              test = import ./test { inherit pkgs cardano; };
              research = import ./research { inherit pkgs; };
            };

          apps = {
            purs-docs = (psProjectFor pkgs).launchSearchablePursDocs { };
          };

          temp = (psProjectFor pkgs);
          temp1 = onchain-outputs;

        }
      );
}
