{ inputs, self, ... }:
{

  flake.overlays.default = final: prev:
    (inputs.ctl.overlays.runtime final prev) //
    (inputs.ctl.overlays.spago final prev) //
    (inputs.ctl.overlays.purescript final prev) //
    {
      cardano-node = self.packages.${final.system}.cardano-node;
      gen-testnet-conf = self.packages.${final.system}.gen-testnet-conf;
      # ogmios = self.packages.${final.system}.ogmios;
      spammer = self.packages.${final.system}.spammer;
      generate-additional-utxo-for-ctl = self.packages.${final.system}.generate-additional-utxo-for-ctl;
    };


  perSystem = { system, ... }: {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      overlays = [ self.overlays.default ];
    };
  };
}
