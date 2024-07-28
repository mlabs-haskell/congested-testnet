{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs, ... }: {
    packages.spammer =
      pkgs.writeShellApplication {
        name = "spammer";
        runtimeInputs = [ pkgs.coreutils pkgs.iputils ];
        text = ''
          ${self'.packages.gen-wallet}/bin/gen-wallet "$1" "$2" 
          echo "==== start ctl spammer ==========="
          export NODE_PATH="${self'.packages.nodeModules}/lib/node_modules"
          while true; do
            ${pkgs.nodejs}/bin/node -e 'require("${self'.packages.compiled}/output/Spammer").main()' 
          done
        '';
      };
  };
}
