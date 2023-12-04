#!/bin/sh
OGMIOS_REQUEST='{"params":{},"method":"queryLedgerState/utxo","jsonrpc":"2.0","id":"queryLedgerState/protocolParameters-5pyr568mlp9m1h8a"}'
echo "$OGMIOS_REQUEST" | tr -d "\n" | websocat ws://127.0.0.1:1337  | jq


OGMIOS_REQUEST_2='{"params":{},"method":"queryLedgerState/protocolParameters","jsonrpc":"2.0","id":"1"}'
echo "$OGMIOS_REQUEST_2" | tr -d "\n" | websocat ws://127.0.0.1:1337  | jq '.result.version'
