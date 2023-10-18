{
  description = "onchain";


  inputs = {
    plutus.url = "github:input-output-hk/plutus/a49a91f467930868a3b6b08f194d94ae3f0e086e";
    iogx.follows = "plutus/iogx";
    hackage.follows = "plutus/hackage";
    CHaP.follows = "plutus/CHaP";
    haskell-nix.follows = "plutus/haskell-nix";

    # hackage = {
    #   url = "github:input-output-hk/hackage.nix/ad01b49b5be1112aed7ad135bf667b7b92169ce1";
    #   flake = false;
    # };

    # CHaP = {
    #   url = "github:input-output-hk/cardano-haskell-packages/4c55186c53103fee3e3973d70a9ce8a3a55a8486";
    #   flake = false;
    # };
    
    # haskell-nix = {
    #   url = "github:input-output-hk/haskell.nix/9be017fdfbd2b290f1df4385ccd0fc22f549c1f2";
    #   inputs.hackage.follows = "hackage";
    # };
  };


  outputs = inputs: inputs.iogx.lib.mkFlake {

    inherit inputs;

    repoRoot = ./.;

    outputs = import ./nix/outputs.nix;

    # systems = [ "x86_64-linux" "x86_64-darwin" ];

    # debug = false;
    # nixpkgsArgs = {
    #   config = {};
    #   overlays = [];
    # };

    # flake = {};
  };


  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}
