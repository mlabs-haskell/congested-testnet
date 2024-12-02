{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs, ... }:
    {
      packages.get-tada = pkgs.writeShellApplication {
        name = "get-tada";
        runtimeInputs = [pkgs.cardano-node pkgs.jq pkgs.curl];
        text = ''
        # we can generate key pairs with cardano-cli

        cardano-cli address key-gen \
            --verification-key-file "key.vkey" \
            --signing-key-file "key.skey" 


        # to get tADA we need to provide public key hash
        PUBKEYHASHHEX=$(cardano-cli address key-hash --payment-verification-key-file "key.vkey")


        # now get ada with query
        curl -X POST "congested-testnet.staging.mlabs.city:8000" -H "Content-Type: application/json" -d "{\"pubKeyHashHex\": \"$PUBKEYHASHHEX\"}"
        # curl -X POST "0.0.0.0:8000" -H "Content-Type: application/json" -d "{\"pubKeyHashHex\": \"$PUBKEYHASHHEX\"}"
        '';
        };
    };
}
