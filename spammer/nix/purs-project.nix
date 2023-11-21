inputs@{ ... }:
let
  inherit (inputs) pkgs;
in

pkgs:
pkgs.purescriptProject rec {
  inherit pkgs;
  projectName = "spammer";
  packageJson = ../spammer/package.json;
  packageLock = ../spammer/package-lock.json;
  src = builtins.path {
    path = ../spammer;
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
}
          

