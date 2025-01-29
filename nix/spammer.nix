{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs, ... }:
    let
      psProjectFor = pkgs: pkgs.purescriptProject rec {
        projectName = "spammer";
        inherit pkgs;
        packageJson = ../spammer/spammer/package.json;
        packageLock = ../spammer/spammer/package-lock.json;
        src = builtins.path {
          path = ../spammer/spammer;
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
      psProject = psProjectFor pkgs;
    in
    {
      devShells.ctl = psProject.devShell;
      packages.compiled = psProject.buildPursProject { };
      packages.nodeModules = psProject.mkNodeModules { };
      packages.spammer = pkgs.stdenv.mkDerivation {
        name = "spammer";
        src = ../spammer/spammer;
        nativeBuildInputs = [
          pkgs.nodejs
        ];
        buildPhase = ''
          export XDG_CACHE_HOME=$TMPDIR/cache
          mkdir -p $XDG_CACHE_HOME
          mkdir -p $out/output
          mkdir -p $out/bin
          mkdir -p $out/src
          cp -r ${self'.packages.compiled}/output/* $out/output
          cp -r $src/src/* $out/src/
          cp -r ${self'.packages.nodeModules}/lib/node_modules $out/node_modules
        '';
        installPhase = ''
          echo "#!/bin/sh" > $out/bin/spammer
          echo "${pkgs.nodejs}/bin/node $out/src/main.js" >> $out/bin/spammer
          chmod +x $out/bin/spammer
        '';
      };
    };
}
