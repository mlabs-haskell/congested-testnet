{ pkgs, tags, cardano, gen-testnet-config}:
let
  inherit (tags) cardano-tag;
in
# pkgs.writeScriptBin "runnet" ''
#   export CARDANO_CLI=${cardano}
#   export ROOT=$(git rev-parse --show-toplevel)
#   export CARDANO_TAG=${cardano-tag}
#   ${./runnet.sh}
# ''
pkgs.writeShellApplication {
    name = "runnet";
    runtimeInputs = [pkgs.git];
    text = ''
    #!/bin/sh
    set +o pipefail
    # shellcheck disable=SC2034,SC2086,SC1072,SC2046
    # move to root
    TESTNET_CONFIG="$(git rev-parse --show-toplevel)/cardano-conf"

    # start docker daemon
    sudo systemctl start docker

    # shutdown all containers 
    # shellcheck disable=SC2046
    # sudo docker stop $(sudo docker ps -a -q)
    # shellcheck disable=SC2046
    # sudo docker volume rm $(sudo docker volume ls -q)
    # sudo docker-compose down --file ${./docker-compose.yaml}

    # add configs to start network
    ${gen-testnet-config}/bin/conf


    # sudo CARDANO_TAG=${cardano-tag} TESTNET_CONFIG=$TESTNET_CONFIG docker compose --file ${./docker-compose.yaml} up -d --remove-orphans --force-recreate --build
    '';
}
     # sudo docker compose --file cluster/docker-compose.yaml exec test-network bash
     # sudo docker compose --file cluster/docker-compose.yaml down -v
     # sudo docker compose --file cluster/docker-compose.yaml logs node-spo-1 
