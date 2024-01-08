@test 'request ogmios' {
OGMIOS_REQUEST='{"method":"queryLedgerState/tip","jsonrpc":"2.0","id":null}'
RESULT=$(echo "$OGMIOS_REQUEST" | tr -d "\n" | websocat ws://0.0.0.0:11337 )
if [[ $(echo $RESULT | jq '.result.slot // empty') -gt 0 ]]; then
    echo "result.slot is greater than 0"
else
   echo "result.slot is not greater than 0"
   exit 1
fi
}

@test 'request kupo' {
curl http://0.0.0.0:11442/matches
}

@test 'request faucet' {
curl http://0.0.0.0:18000
}

@test 'check spammer works based on mempool usage in prometheus statistics' {
RESULT=$(curl 'http://0.0.0.0:19090/api/v1/query?query=round%28avg_over_time%28cardano_node_metrics_mempoolBytes_int%5B1h%5D%29%29')
val=$(echo $RESULT | jq '.data.result.[0].value.[0]')
if [[$val -gt 1]]; then
    echo "ok"
else 
   exit 1
fi

}
