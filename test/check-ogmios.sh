#!/bin/sh
OGMIOS_REQUEST='{ 
  "type": "jsonwsp/request",
  "version": "1.0",
  "servicename": "ogmios",
  "methodname": "Query",
  "args": { "query": "utxo" }
}'
echo "Sending the following request to Ogmios"
echo "$OGMIOS_REQUEST" | jq
echo "OGMIOS RESULT"
echo "$OGMIOS_REQUEST" | tr -d "\n" | websocat ws://127.0.0.1:1337 | jq ".result"

