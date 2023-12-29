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

      packages.faucet =
        let
          compiled = psProject.buildPursProject { };
          nodeModules = psProject.mkNodeModules { };
        in
        pkgs.writeShellApplication {
          name = "faucet";
          runtimeInputs = [ pkgs.coreutils ];
          text = ''
            export NODE_PATH="${nodeModules}/lib/node_modules"
            ${pkgs.nodejs}/bin/node -e 'require("${compiled}/output/Faucet").main()' 
          '';
        };
      packages.ogmios = inputs.ctl.inputs.ogmios-nixos.packages.${system}."ogmios:exe:ogmios";
      apps.purs-docs = psProject.launchSearchablePursDocs { };
    };
}
