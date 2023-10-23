#!/bin/sh
cd $ROOT
echo "start docker daemon"
sudo systemctl start docker
echo "shutdown old testnet"
sudo docker compose --file cluster/docker-compose.yaml down
echo "generate foler with configuration and sockets"
sudo rm -rf cardano-conf
echo "add configs to start network"
nix run .#config
echo "start docker-compose environment"
sudo docker compose --file cluster/docker-compose.yaml up -d --remove-orphans --force-recreate --build



# sudo docker compose --file cluster/docker-compose.yaml exec test-network bash
# sudo docker compose --file cluster/docker-compose.yaml down
# sudo docker compose --file cluster/docker-compose.yaml logs node-spo-1 

