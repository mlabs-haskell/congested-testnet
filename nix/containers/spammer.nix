{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs, ... }: {

    # packages.spammer =
    #   pkgs.writeShellApplication {
    #     name = "spammer-and-faucet";
    #     runtimeInputs = [ pkgs.coreutils pkgs.iputils ];
    #     text = ''
    #       export walletPath="/tmp/wallet"
    #       export ogmiosUrl="0.0.0.0" 
    #       export kupoUrl="0.0.0.0"
    #       # start spammer and faucet in parallel 
    #       export NODE_PATH="${self'.packages.nodeModules}/lib/node_modules"
    #      ${pkgs.nodejs}/bin/node -e 'import("${self'.packages.compiled1}/output/Main/index.js").then(m => m.main())'
    #     '';
    #   };

    packages.spammer =  pkgs.stdenv.mkDerivation {
     name = "spammer-and-faucet";
     src = ../../spammer/spammer;
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
        cp $src/src/main.js $out/src/main.js
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
