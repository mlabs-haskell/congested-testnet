{ inputs, self, ... }:
{
  perSystem = { system, ... }:
    let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.ctl.overlays.purescript
          inputs.ctl.overlays.runtime
          inputs.ctl.overlays.spago
        ];
      };
      psProject = (import ./functions.nix).psProjectFor self pkgs;
    in
    {
      devShells.ctl = psProject.devShell;
      packages.faucet = psProject.buildPursProject {
      # main = "Faucet";
      }; 
      apps.purs-docs = psProject.launchSearchablePursDocs { };
    };
}
