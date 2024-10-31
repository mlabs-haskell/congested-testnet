{ inputs, self, ... }:
{

flake.overlays.default = final: prev: {
    gen-testnet-conf = self.packages.${final.system}.gen-testnet-conf;
    ogmios-run = self.packages.${final.system}.ogmios-run;
    kupo-run = self.packages.${final.system}.kupo-run;
    gen-wallet = self.packages.${final.system}.gen-wallet;
    spammer = self.packages.${final.system}.spammer;
    make-faucet-wallet = self.packages.${final.system}.make-faucet-wallet;
    relay-node = self.packages.${final.system}.relay-node;
    spo-node = self.packages.${final.system}.spo-node;
    prometheus-run = self.packages.${final.system}.prometheus-run;
    arion-with-prebuilt = self.packages.${final.system}.arion-with-prebuilt;
    arion-with-prebuilt-dev = self.packages.${final.system}.arion-with-prebuilt-dev;
    cardano-cli-remote-container = self.packages.${final.system}.cardano-cli-remote-container;
    update-kes = self.packages.${final.system}.update-kes;
    cardano-node = self.packages.${final.system}.cardano-node-1;
    # cardano-cli = self.packages.${final.system}.cardano-cli;
    share-config = self.packages.${final.system}.share-config;
    copy-config = self.packages.${final.system}.copy-config;
    add-ping = self.packages.${final.system}.add-ping;
    podman = inputs.arion.inputs.nixpkgs.legacyPackages.${final.system}.podman;
    docker = inputs.arion.inputs.nixpkgs.legacyPackages.${final.system}.docker;
    docker-client = inputs.arion.inputs.nixpkgs.legacyPackages.${final.system}.docker-client;
    docker-compose = inputs.arion.inputs.nixpkgs.legacyPackages.${final.system}.docker-compose;
    arion = inputs.arion.inputs.nixpkgs.legacyPackages.${final.system}.arion;
    faucet = self.packages.${final.system}.faucet;
  };

perSystem = { system, ... }: {
  _module.args.pkgs = import inputs.nixpkgs { inherit system; overlays = 
  [
  inputs.ctl.overlays.runtime
  inputs.ctl.overlays.spago
  inputs.ctl.overlays.purescript
  self.overlays.default
  ]; 
  };
};
}
