#!/bin/sh
cd $ROOT
# start docker daemon
sudo systemctl start docker
# shutdown old testnet
sudo docker compose --file cluster/docker-compose.yaml down -v
# generate foler with configuration and sockets
sudo rm -rf cardano-conf
# add configs to start network
nix run .#config
# start docker-compose environment
sudo CARDANO_TAG=$CARDANO_TAG docker compose \
  --file cluster/docker-compose.yaml \ 
  --file cluster/
  up -d --remove-orphans --force-recreate --build



# sudo docker compose --file cluster/docker-compose.yaml exec test-network bash
# sudo docker compose --file cluster/docker-compose.yaml down -v
# sudo docker compose --file cluster/docker-compose.yaml logs node-spo-1 

