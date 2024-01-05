{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs', ... }:{
      packages.spammer =
        pkgs'.writeShellApplication {
          name = "spammer";
          runtimeInputs = [ pkgs'.coreutils ];
          text = ''
            ${self'.packages.gen-wallet}/bin/gen-wallet "$1" 
            sleep 3
            echo "==== start ctl spammer ==========="
            export NODE_PATH="${self'.packages.nodeModules}/lib/node_modules"
            ${pkgs'.nodejs}/bin/node -e 'require("${self'.packages.compiled}/output/Main").main()' 
            echo "=================================="
          '';
        };
    };
}
