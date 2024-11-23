{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs, ... }:
    let
      psProject = (import ./functions.nix).psProjectFor pkgs;
    in
    {
      devShells.ctl = psProject.devShell;
      packages.compiled = psProject.buildPursProject { };
      packages.nodeModules = psProject.mkNodeModules { };
    };
}
