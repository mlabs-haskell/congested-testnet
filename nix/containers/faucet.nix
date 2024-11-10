{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs, ... }: {
    packages.faucet =
      pkgs.writeShellApplication {
        name = "faucet";
        runtimeInputs = [ pkgs.coreutils ];
        text = ''
          ${self'.packages.make-faucet-wallet}/bin/make-faucet-wallet wallet socket config 
        '';
      };
  };
}
