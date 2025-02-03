{ inputs, self, ... }:
{
  perSystem = { system, inputs', pkgs, self', ... }:
    {
      packages.gen-testnet-conf = pkgs.writeShellApplication {
        name = "gen-testnet-conf";
        runtimeInputs = with pkgs; [
          cardano-node
          jq
          tree
        ];
        text = ''
          ROOT=$1
          BYRON_GENESIS_SPEC_JSON=${../scripts/byron.genesis.spec.json}
          ${../scripts/gen_testnet_conf.sh} "$ROOT" "$BYRON_GENESIS_SPEC_JSON"
        '';
      };


