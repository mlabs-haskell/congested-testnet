{ inputs, self, ... }:
{
perSystem = { system, self', pkgs, ... }: {
  _module.args.pkgs = import inputs.nixpkgs {
    inherit system;
    overlays = [
      inputs.ctl.overlays.runtime
      inputs.ctl.overlays.spago
      inputs.ctl.overlays.purescript
      (final: prev: {
          compiled = self'.packages.compiled;
        }
      )
    ];
  };
};
}
