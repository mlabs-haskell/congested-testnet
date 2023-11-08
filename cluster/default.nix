{ pkgs, tags }:
let
  inherit (tags) cardano-tag;
in
pkgs.writeScriptBin "runnet" ''
  export ROOT=$(git rev-parse --show-toplevel)
  export CARDANO_TAG=${cardano-tag}
  ${./runnet.sh}
''
