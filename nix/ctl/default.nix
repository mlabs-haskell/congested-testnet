{ inputs, self, ... }:
{
  perSystem = { system, self', ... }:
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
      packages.compiled = psProject.buildPursProject { };
      packages.nodeModules = psProject.mkNodeModules { };
      packages.ogmios = inputs.ctl.inputs.ogmios-nixos.packages.${system}."ogmios:exe:ogmios";
      packages.kupo = inputs.ctl.inputs.kupo-nixos.packages.${system}.kupo;
      apps.purs-docs = psProject.launchSearchablePursDocs { };
    };
}
