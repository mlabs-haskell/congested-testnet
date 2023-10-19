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


  outputs = { self, nixpkgs, flake-utils, cardano, iohk-nix, ... }@inputs:
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
          ];
          pkgs = import nixpkgs {
            inherit system overlays; config.permittedInsecurePackages =
            [
              "nodejs-14.21.3"
              "openssl-1.1.1w"
            ];
            config.allowBroken = true;
          };

          devShell = with pkgs; mkShell {
            buildInputs = [
              pkgs.nixpkgs-fmt
              cardano.legacyPackages.${system}.cardano-cli
              cardano.legacyPackages.${system}.cardano-node
              postgresql_14
            ] ++ (with pkgs.python310Packages; [ jupyterlab pandas psycopg2 ])
            ++ onchain-outputs.devShell.${system}.buildInputs;
            # ++ spamer-ctl.outputs.devShells.${system}.default.buildInputs
            shellHook = ''
            '';
          };

        in
        {
          devShells.default = devShell;
          packages = onchain-outputs.packages.${system} //
            { config = import ./config { inherit pkgs iohk-nix cardano system; }; };
        }
      );
}
