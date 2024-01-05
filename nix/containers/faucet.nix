{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs', ... }:{
      packages.faucet =
        pkgs'.writeShellApplication {
          name = "faucet";
          runtimeInputs = [ pkgs'.coreutils ];
          text = ''
            export NODE_PATH="${self'.packages.nodeModules}/lib/node_modules"
            ${pkgs'.nodejs}/bin/node -e 'require("${self'.packages.compiled}/output/Faucet").main()' 
          '';
        };
    };
}
