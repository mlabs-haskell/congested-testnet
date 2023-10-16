#!/bin/sh
echo
OGMIOS_REQUEST='{ 
  "type": "jsonwsp/request",
  "version": "1.0",
  "servicename": "ogmios",
  "methodname": "Query",
  "args": { "query": "utxo" }
}'
echo "Sending the following request to Ogmios to get UTXOs"
echo "$OGMIOS_REQUEST" | jq
echo "UTXOs:"
echo "$OGMIOS_REQUEST" | tr -d "\n" | websocat ws://localhost:1337 | jq ".result"

