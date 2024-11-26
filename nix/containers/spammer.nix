{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs, ... }: {

    packages.spammer =
      pkgs.writeShellApplication {
        name = "spammer-and-faucet";
        runtimeInputs = [ pkgs.coreutils pkgs.iputils ];
        text = ''
          export walletPath="/tmp/wallet"
          export ogmiosUrl="0.0.0.0" 
          export kupoUrl="0.0.0.0"
          # start spammer and faucet in parallel 
          export NODE_PATH="${self'.packages.nodeModules}/lib/node_modules"
         ${pkgs.nodejs}/bin/node -e 'import("${self'.packages.compiled1}/output/Main/index.js").then(m => m.main())'

        '';
      };
    packages.spammer2 =
      pkgs.writeShellApplication {
        name = "spammer-and-faucet";
        runtimeInputs = [
          # pkgs.easy-ps.purs-0_15_8
          # pkgs.easy-ps.psa 
          # pkgs.purs 
          # pkgs.easy-ps.spago
         pkgs.nodejs
          ];
        text = ''
          export walletPath="/tmp/wallet"
          export ogmiosUrl="0.0.0.0" 
          export  kupoUrl="0.0.0.0"
          tmpDir=$(mktemp -d /tmp/wallet-XXXXXX)
          cp -r ${../../spammer/spammer}/src "$tmpDir"/
          cp -r ${self'.packages.compiled1}/output "$tmpDir"/output
          cp -r ${self'.packages.nodeModules}/lib/node_modules "$tmpDir"/node_modules
          # start spammer and faucet in parallel 
          cd "$tmpDir"
          node src/main.js
          echo "hi"
        '';
      };

          # ${pkgs.nodejs}/bin/node -e 'require("${self'.packages.compiled}/output/Main").main()' 
    packages.compiled1 =  pkgs.stdenv.mkDerivation {
     name = "compiled1";
     src = ../../spammer/spammer;
      nativeBuildInputs = [
        pkgs.tree
      ];
      buildPhase = ''
        export XDG_CACHE_HOME=$TMPDIR/cache
        mkdir -p $XDG_CACHE_HOME
        mkdir -p $out/output
        cp -r ${self'.packages.compiled}/output/* $out/output
        cp $src/src/workers.js $out/output/
      '';
      installPhase = ''
       echo hello
      '';
    };
    # in
    # compiled;
    # pkgs.writeShellApplication {
    #   name = "spammer-and-faucet";
    #   runtimeInputs = [ 
    #     pkgs.easy-ps.psa
    #     pkgs.easy-ps.spago
    #     pkgs.tree
    #     pkgs.nodejs
    #   ];
    #   text = ''
    #     export walletPath="/tmp/wallet"
    #     export ogmiosUrl="0.0.0.0" 
    #     export kupoUrl="0.0.0.0"
    #     ls ${compiled}/spammer
    #   '';
    # };



  };
}
        # cp -r ${self'.packages.compiled}/output/* output/
        # cp $src/main.js output/src 
        # start spammer and faucet in parallel 
        # export NODE_PATH="${self'.packages.nodeModules}/lib/node_modules"
        # node ${compiled}/output/src/main.js
        # export NODE_PATH="${self'.packages.nodeModules}/lib/node_modules"
        # ${pkgs.nodejs}/bin/node -e 'require("${self'.packages.compiled}/output/Main").main()' 
  # };
          # ${pkgs.nodejs}/bin/node -e 'require("${self'.packages.compiled}/output/Main").main()' 
          # ${pkgs.nodejs}/bin/node -e '
          #     (async () => {
          #       const mainModule = await import("${self'.packages.compiled}/output/Main");
          #       mainModule.main();
          #     })();
          # ' 
          # ${pkgs.nodejs}/bin/node -e 'require("${self'.packages.compiled}/output/Main").main()' 
        #   ${pkgs.nodejs}/bin/node -e '
        #       (async () => {
        #         const {main} = await import("${self'.packages.compiled}/output/Main/foreign.js");
        #         main();
        #       })();
        #   ' 
