{ inputs, self, ... }:
{

flake.overlays.default = final: prev: 
  (inputs.ctl.overlays.runtime final prev) //
  (inputs.ctl.overlays.spago final prev) //
  (inputs.ctl.overlays.purescript final prev) //
    {
    cardano-node = self.packages.${final.system}.cardano-node;
    gen-testnet-conf = self.packages.${final.system}.gen-testnet-conf;
    ogmios = self.packages.${final.system}.ogmios;
    # ogmios-run = self.packages.${final.system}.ogmios-run;
    # kupo-run = self.packages.${final.system}.kupo-run;
    gen-wallet = self.packages.${final.system}.gen-wallet;
    spammer = self.packages.${final.system}.spammer;
    make-faucet-wallet = self.packages.${final.system}.make-faucet-wallet;
    relay-node = self.packages.${final.system}.relay-node;
    spo-node = self.packages.${final.system}.spo-node;
    prometheus-run = self.packages.${final.system}.prometheus-run;
    # arion-with-prebuilt = self.packages.${final.system}.arion-with-prebuilt;
    # arion-with-prebuilt-dev = self.packages.${final.system}.arion-with-prebuilt-dev;
    cardano-cli-remote-container = self.packages.${final.system}.cardano-cli-remote-container;
    share-config = self.packages.${final.system}.share-config;
    copy-config = self.packages.${final.system}.copy-config;
    generate-additional-utxo-for-ctl = self.packages.${final.system}.generate-additional-utxo-for-ctl;
    podman = inputs.arion.inputs.nixpkgs.legacyPackages.${final.system}.podman;
    docker = inputs.arion.inputs.nixpkgs.legacyPackages.${final.system}.docker;
    docker-client = inputs.arion.inputs.nixpkgs.legacyPackages.${final.system}.docker-client;
    docker-compose = inputs.arion.inputs.nixpkgs.legacyPackages.${final.system}.docker-compose;
    arion = inputs.arion.inputs.nixpkgs.legacyPackages.${final.system}.arion;
    faucet = self.packages.${final.system}.faucet;
  };


perSystem = { system, ... }: {
  _module.args.pkgs = import inputs.nixpkgs { 
  inherit system; 
  overlays = [ self.overlays.default ]; 
  };
};
}
