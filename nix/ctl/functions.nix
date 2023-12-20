{
  psProjectFor = self: pkgs: pkgs.purescriptProject rec {
    inherit pkgs; projectName = "spammer";
    packageJson = "${self.outPath}/spammer/spammer/package.json";
    packageLock = "${self.outPath}/spammer/spammer/package-lock.json";
    src = builtins.path {
      path = "${self.outPath}/spammer/spammer";
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
