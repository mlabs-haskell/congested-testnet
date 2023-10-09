#!/bin/sh
sudo rm -rf cardano-conf
nix run .#config
sudo docker compose --file cluster/docker-compose.yaml up -d --remove-orphans --force-recreate --build


# sudo docker compose --file cluster/docker-compose.yaml exec test-network bash
# sudo docker compose --file cluster/docker-compose.yaml down
# sudo docker compose --file cluster/docker-compose.yaml logs node-spo-1 

