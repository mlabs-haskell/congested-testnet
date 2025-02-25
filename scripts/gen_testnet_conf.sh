#!/bin/sh
# defined outside vars:
# export BYRON_GENESIS_SPEC_JSON
# export CONFIGURATION_YAML
# export ROOT
# export SHARE 
# export MAX_BLOCK_BODY_SIZE 
# export SLOT_LENGTH 

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

jq --argjson maxSupply "$MAX_SUPPLY" \
   --argjson secParam "$SECURITY_PARAM" \
   --argjson maxBody "$MAX_BLOCK_BODY_SIZE" \
   --argjson slotLength "$SLOT_LENGTH" \
  '.maxLovelaceSupply = $maxSupply 
  | .slotLength = $slotLength 
  | .securityParam = $secParam 
  | .activeSlotsCoeff = 0.05 
  | .epochLength = 432000 
  | .updateQuorum = 2 
  | .protocolParams.protocolVersion.major = 9 
  | .protocolParams.minFeeA = 44 
  | .protocolParams.minFeeB = 155381 
  | .protocolParams.minUTxOValue = 1000000 
  | .protocolParams.decentralisationParam = 1 
  | .protocolParams.rho = 0.003 
  | .protocolParams.tau = 0.2 
  | .protocolParams.a0 = 0.3 
  | .protocolParams.maxBlockBodySize = $maxBody 
  | .protocolParams.maxBlockHeaderSize = 1100' "$ROOT/shelley-genesis.json" > "$ROOT/temp.json" && mv "$ROOT/temp.json" "$ROOT/shelley-genesis.json"



# copy configuragtion yaml
cp $CONFIGURATION_YAML $ROOT/configuration.yaml

# copy some config files to share directory
mkdir -p "$SHARE/byron-gen-command"
cp "$ROOT/shelley-genesis.json" "$SHARE"
cp "$ROOT/byron-gen-command/genesis.json" "$SHARE/byron-gen-command/genesis.json"
cp "$ROOT/conway-genesis.json" "$SHARE"
cp "$ROOT/alonzo-genesis.json" "$SHARE"
cp "$ROOT/configuration.yaml" "$SHARE"


touch "$ROOT/config.finish"

if [ ! -f "$ROOT/config.finish" ]; then
  echo "error in gen testnet conf"
  exit 1
fi
