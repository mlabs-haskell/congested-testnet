{ pkgs, cardano-cli }:
rec {
  first-transaction = pkgs.writeScriptBin "first-transaction" ''
    cd $(git rev-parse --show-toplevel)
    export CARDANO_CLI=${cardano-cli}
    alias cardano-cli=${cardano-cli}/bin/cardano-cli 
    ${./ada-transfer-from-genesis-to-wallet0.sh} 
  '';

}
