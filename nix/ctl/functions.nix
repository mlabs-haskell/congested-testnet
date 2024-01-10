{
  psProjectFor = pkgs: pkgs.purescriptProject rec {
    projectName = "spammer";
    inherit pkgs;
    packageJson = ../../spammer/spammer/package.json;
    packageLock = ../../spammer/spammer/package-lock.json;
    src = builtins.path {
      path = ../../spammer/spammer;
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
  };
}
