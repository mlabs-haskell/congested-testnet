{
  description = "congested-testnet";
  inputs.nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
  inputs.flake-utils.url = github:numtide/flake-utils;
  inputs.cardano.url = github:input-output-hk/cardano-node/8.4.0-pre;
  inputs.iohk-nix.url = github:input-output-hk/iohk-nix;

  outputs = { self, nixpkgs, flake-utils, cardano, iohk-nix, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          overlays = [ ];
          pkgs = import nixpkgs {
            inherit system overlays;
          };

          devShell = with pkgs; mkShell {
            buildInputs = [
              pkgs.nixpkgs-fmt
              cardano.legacyPackages.${system}.cardano-cli
              cardano.legacyPackages.${system}.cardano-node
              pkgs.yq
            ];
            shellHook = ''
            '';
          };

        in
        {
          devShells.default = devShell;
          packages = {
           config = import ./config {inherit pkgs iohk-nix;};
          };
        }
      );
}
