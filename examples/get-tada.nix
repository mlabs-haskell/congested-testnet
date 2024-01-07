{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs, ... }:
    {
      packages.get-tada = pkgs.writeShellApplication {
        name = "get-tada";
        runtimeInputs = [self'.packages.cardano-node pkgs.jq pkgs.curl];
        text = ''
        # we can generate key pairs with cardano-cli

        cardano-cli address key-gen \
            --verification-key-file "key.vkey" \
            --signing-key-file "key.skey" 


        # to get tADA we need to provide public key
        PUBKEYHEX=$( jq '.cborHex' < "key.vkey" ) 


        # now get ada with curl query
        curl -X POST "faucet.congested-testnet.staging.mlabs.city:8000" -H "Content-Type: application/json" -d "{\"pubKeyHex\": $PUBKEYHEX}"
        '';
        };
    };
}
