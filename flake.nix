{
  description = "congested-testnet";
  inputs.flake-utils.url = github:numtide/flake-utils;
  inputs.cardano.url = github:input-output-hk/cardano-node/8.1.2;
  inputs.nixpkgs.follows = "cardano/nixpkgs";
  inputs.iohk-nix.url = github:input-output-hk/iohk-nix/v2.2;
  # inputs.spamer-ctl.url = "path:./spamer/basic-spamer-ctl";

  inputs.plutus.url = "github:input-output-hk/plutus/a49a91f467930868a3b6b08f194d94ae3f0e086e";
  inputs.iogx.follows = "plutus/iogx";
  inputs.hackage.follows = "plutus/hackage";
  inputs.CHaP.follows = "plutus/CHaP";
  inputs.haskell-nix.follows = "plutus/haskell-nix";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  inputs.ctl = {
    type = "github";
    owner = "Plutonomicon";
    repo = "cardano-transaction-lib";
    rev = "605931759ff35bdd71bb4d933071aced9fb57870";
  };


  outputs = { self, nixpkgs, flake-utils, cardano, iohk-nix, ctl, ... }@inputs:
    let
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
              "openssl-1.1.1t"
            ];
            config.allowBroken = true;
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
              cardano.legacyPackages.${system}.cardano-cli
              cardano.legacyPackages.${system}.cardano-node
              postgresql_14
            ] ++ (with pkgs.python310Packages; [ jupyterlab pandas psycopg2 ])
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
            rec {
              # generate config files for testnet
              config = import ./config { inherit pkgs iohk-nix cardano system; };
              ctl-node = (psProjectFor pkgs).buildPursProject {
                main = "Spamer.Main";
                entrypoint = "index.js";
              };
              # run testnet with docker compose
              runnet = import ./cluster { inherit pkgs; };
              test = import ./test { inherit pkgs; cardano-cli = cardano.legacyPackages.${system}.cardano-cli; };

            };

          apps = {
            purs-docs = (psProjectFor pkgs).launchSearchablePursDocs { };
          };

        }
      );
}
