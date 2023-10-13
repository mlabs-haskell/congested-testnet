{
  description = "congested-testnet";
  inputs.nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
  inputs.flake-utils.url = github:numtide/flake-utils;
  inputs.cardano.url = github:input-output-hk/cardano-node/8.1.2;
  inputs.iohk-nix.url = github:input-output-hk/iohk-nix/v2.2;
  inputs.spamer.url = "path:./spamer/basic-spamer-ctl";


  outputs = { self, nixpkgs, flake-utils, cardano, iohk-nix, spamer,  ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          overlays = [ 
          ];
          pkgs = import nixpkgs {
            inherit system overlays; config.permittedInsecurePackages = 
            ["nodejs-14.21.3"
            "openssl-1.1.1w"]; config.allowBroken = true;
          };

          devShell = with pkgs; mkShell {
            buildInputs = [
              pkgs.nixpkgs-fmt
              cardano.legacyPackages.${system}.cardano-cli
              cardano.legacyPackages.${system}.cardano-node
              # easy-ps.purescript-language-server
              # easy-ps.purs-tidy
              # easy-ps.spago
              postgresql_14
            ] ++ (with pkgs.python310Packages; [jupyterlab pandas psycopg2]);
            shellHook = ''
            '';
          };

        in
        {
          devShells.default = devShell // spamer.outputs.devShells.${system}.default; 
          packages = {
            config = import ./config { inherit pkgs iohk-nix cardano system; };
          };
        }
      );
}
