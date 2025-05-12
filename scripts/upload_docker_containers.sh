#!/bin/sh

nix build .#congested-testnet-image
docker load < result
docker login ghcr.io -u sadMaxim --password $(cat ../../../secrets/GIT_TOKEN) 
docker tag cgnet:latest ghcr.io/mlabs-haskell/cgnet:latest
docker push ghcr.io/mlabs-haskell/cgnet:latest
