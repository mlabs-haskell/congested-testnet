{ pkgs, cardano}:
rec {
  first-transaction = pkgs.writeScriptBin "first-transaction" ''
    cd $(git rev-parse --show-toplevel)
    export CARDANO_CLI=${cardano}
    alias cardano-cli=${cardano}/bin/cardano-cli 
    ${./ada-transfer-from-genesis-to-wallet0.sh} 
  '';
  wallet0-wallet0-transaction = pkgs.writeScriptBin "user-user-transaction" ''
    cd $(git rev-parse --show-toplevel)
    export CARDANO_CLI=${cardano}
    alias cardano-cli=${cardano}/bin/cardano-cli 
    ${./ada-transfer-from-wallet0-wallet0.sh} 
  '';

}
