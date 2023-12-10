#!/bin/sh
nix run .#runnet start
sleep 1
nix run .#first-transaction

cd spammer/spammer
spago run -m InsertScripts -b /tmp/scripts.txt
cd ../..
ogmios --host 0.0.0.0 --node-socket cardano-conf/sockets/node-relay-1-socket/node.socket --node-config cardano-conf/configuration.yaml


