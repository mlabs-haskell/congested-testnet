{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs, ... }: {
    packages.faucet =
      pkgs.writeShellApplication {
        name = "faucet";
        runtimeInputs = [ pkgs.coreutils ];
        text = ''
          ${self'.packages.make-faucet-wallet}/bin/make-faucet-wallet wallet socket config 
          export NODE_PATH="${self'.packages.nodeModules}/lib/node_modules"
          ${pkgs.nodejs}/bin/node -e 'require("${self'.packages.compiled}/output/Faucet").main()' 
        '';
      };
  };
}
