#!/bin/sh
# defined outside vars:
# export BYRON_GENESIS_SPEC_JSON
# export CONFIGURATION_YAML
# export ROOT
# export SHARE 
#

mkdir -p $ROOT

if [ -f "$ROOT/config.finish" ]; then
  echo "config exists. exiting..."
  exit 0
fi

NETWORK_MAGIC=42
SECURITY_PARAM=2160
NUM_SPO_NODES=1
INIT_SUPPLY=17440737095516159
START_TIME="$(date -d "now + 2 seconds" +%s)" 



cardano-cli byron genesis genesis --protocol-magic "$NETWORK_MAGIC" \
 --start-time "$START_TIME" \
 --k "$SECURITY_PARAM" \
 --n-poor-addresses 0 \
 --n-delegate-addresses "$NUM_SPO_NODES" \
 --total-balance "$INIT_SUPPLY" \
 --delegate-share 1 \
 --avvm-entry-count 0 \
 --avvm-entry-balance 0 \
 --protocol-parameters-file "$BYRON_GENESIS_SPEC_JSON" \
 --genesis-output-dir "$ROOT/byron-gen-command"



MAX_SUPPLY=18346744073709551615


cardano-cli conway genesis create-testnet-data \
        --genesis-keys 1 \
        --pools 1\
        --stake-delegators 1\
        --utxo-keys 1 \
        --testnet-magic 42 \
        --total-supply "$MAX_SUPPLY" \
        --out-dir "$ROOT" 

# change some parameters in shelley genesis

jq --argjson maxSupply "$MAX_SUPPLY" --argjson secParam "$SECURITY_PARAM" '.maxLovelaceSupply = $maxSupply | .slotLength = 2 | .securityParam = $secParam | .activeSlotsCoeff = 0.05 | .epochLength = 432000 | .updateQuorum = 2 | .protocolParams.protocolVersion.major = 9 | .protocolParams.minFeeA = 44 | .protocolParams.minFeeB = 155381 | .protocolParams.minUTxOValue = 1000000 | .protocolParams.decentralisationParam = 1 | .protocolParams.rho = 0.003 | .protocolParams.tau = 0.2 | .protocolParams.a0 = 0.3 | .protocolParams.maxBlockBodySize = 17000 | .protocolParams.maxBlockHeaderSize = 1100' "$ROOT/shelley-genesis.json" > "$ROOT/temp.json" && mv "$ROOT/temp.json" "$ROOT/shelley-genesis.json"


# update plutusV2 costModel from https://book.world.dev.cardano.org/resources/cost-model-fork-conway.json 
# jq '.costModels.PlutusV2 = [100788,420,1,1,1000,173,0,1,1000,59957,4,1,11183,32,201305,8356,4,16000,100,16000,100,16000,100,16000,100,16000,100,16000,100,100,100,16000,100,94375,32,132994,32,61462,4,72010,178,0,1,22151,32,91189,769,4,2,85848,228465,122,0,1,1,1000,42921,4,2,24548,29498,38,1,898148,27279,1,51775,558,1,39184,1000,60594,1,141895,32,83150,32,15299,32,76049,1,13169,4,22100,10,28999,74,1,28999,74,1,43285,552,1,44749,541,1,33852,32,68246,32,72362,32,7243,32,7391,32,11546,32,85848,228465,122,0,1,1,90434,519,0,1,74433,32,85848,228465,122,0,1,1,85848,228465,122,0,1,1,955506,213312,0,2,270652,22588,4,1457325,64566,4,20467,1,4,0,141992,32,100788,420,1,1,81663,32,59498,32,20142,32,24588,32,20744,32,25933,32,24623,32,43053543,10,53384111,14333,10,43574283,26308,10]' "$ROOT/alonzo-genesis.json" > "$ROOT/temp.json" && mv "$ROOT/temp.json" "$ROOT/alonzo-genesis.json"


# copy configuragtion yaml
cp $CONFIGURATION_YAML $ROOT/configuration.yaml

# copy some config files to share directory
mkdir -p "$SHARE/byron-gen-command"
cp "$ROOT/shelley-genesis.json" "$SHARE"
cp "$ROOT/byron-gen-command/genesis.json" "$SHARE/byron-gen-command/genesis.json"
cp "$ROOT/conway-genesis.json" "$SHARE"
cp "$ROOT/alonzo-genesis.json" "$SHARE"
cp "$ROOT/configuration.yaml" "$SHARE"


touch config.finish
