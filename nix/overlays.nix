{ inputs, self, ... }:
{
perSystem = { system, self, ... }: {
  _module.args.pkgs = import inputs.nixpkgs {
    inherit system;
    overlays = [
      inputs.ctl.overlays.runtime
      inputs.ctl.overlays.spago
      inputs.ctl.overlays.purescript
    ];
  };
};
}
