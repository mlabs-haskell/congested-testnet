#!/bin/sh

nix build .#congested-testnet-image
docker load < result
docker login ghcr.io -u sadMaxim --password $(cat ../../../secrets/GIT_TOKEN) 
docker tag cgnet:latest ghcr.io/mlabs-haskell/cgnet:latest
docker push ghcr.io/mlabs-haskell/cgnet:latest

nix build .#congested-testnet-relay-image
docker load < result
docker login ghcr.io -u sadMaxim --password $(cat ../../../secrets/GIT_TOKEN) 
docker tag cgnet-relay:latest ghcr.io/mlabs-haskell/cgnet-relay:latest
docker push ghcr.io/mlabs-haskell/cgnet-relay:latest

nix build .#congested-testnet-example-image
docker load < result
docker login ghcr.io -u sadMaxim --password $(cat ../../../secrets/GIT_TOKEN) 
docker tag cgnet-example:latest ghcr.io/mlabs-haskell/cgnet-example:latest
docker push ghcr.io/mlabs-haskell/cgnet-example:latest
