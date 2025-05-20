@test 'request ogmios' {
OGMIOS_REQUEST='{"method":"queryLedgerState/tip","jsonrpc":"2.0","id":null}'
RESULT=$(echo "$OGMIOS_REQUEST" | tr -d "\n" | websocat ws://localhost:1337 )
if [[ $(echo $RESULT | jq '.result.slot // empty') -gt 0 ]]; then
    echo "result.slot is greater than 0"
else
   echo "result.slot is not greater than 0"
   exit 1
fi
}

@test 'request kupo' {
curl http://localhost:1442/matches
}

@test 'request faucet' {
curl http://localhost:8000
}

@test 'check spammer works based on mempool usage in prometheus statistics' {
RESULT=$(curl 'http://localhost:9090/api/v1/query?query=round%28avg_over_time%28cardano_node_metrics_mempoolBytes_int%5B1h%5D%29%29')
val=$(echo $RESULT | jq '.data.result.[0].value.[0]')
if (( $(echo "$val > 1" | bc -l) )); then
    echo "ok"
else 
    exit 1
fi

}
