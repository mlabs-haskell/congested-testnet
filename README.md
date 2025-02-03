# congested-testnet
The goal of this project is to operate a cardano testnet under constant congestion. Therefore, an additional component, a spammer, is included. This component continuously generates transactions to simulate periods of congestion commonly observed on the mainnet.

- [Congestion research](./docs/congestion-statistics.md)
- [Design](./docs/design.md)
- [How to use](#how-to-use)
## How to Use

The **Genesis SPO node** with a spammer, which simulates network congestion, is running on the server [`congested-testnet.staging.mlabs.city`](http://congested-testnet.staging.mlabs.city) on the following ports:

- `8000` – Faucet  
- `1442` – Kupo  
- `1337` – Ogmios  
- `5000` – Shared testnet configuration files for running a custom node  

<!-- #### Submitting Custom Transactions -->

You can submit custom transactions **without running a local node**. This process is demonstrated in the [`cardano-cli-nodejs` example](./examples/get-ada-submit-tx.js). Additionally, you will find examples on how to request **Faucet, Ogmios, and Kupo**.  

To follow these examples, ensure you have [Cardano Node `10.1.4`](https://github.com/IntersectMBO/cardano-node/releases/tag/10.1.4) installed. Alternatively, you can run our preconfigured environment with `cardano-node nodejs` using Docker:

```bash
docker run -it --rm -v $(pwd)/examples:/examples ghcr.io/mlabs-haskell/cgnet-example:latest node /examples/get-ada-submit-tx.js
```  

Also you can run your own Genesis SPO node with ogmios-kupo-faucet-share_config and simulate congestion using spammer:

```bash
git clone https://github.com/mlabs-haskell/congested-testnet
cd congested-testnet
SPAMMER_ON=true FAUCET_ON=true docker-compose up --profile genesis_spo up -d
```
If you only need to run a relay node connected to the Genesis SPO, use the following command:
```bash
SPO_ADDRESS=http://congested-testnet.staging.mlabs.city docker-compose --profile relay_node up -d
SPO_ADDRESS=http://congested-testnet.staging.mlabs.city docker-compose --profile relay_node down -v
SPO_ADDRESS=http://congested-testnet.staging.mlabs.city docker-compose --profile relay_node logs relay_node 
docker exec -it relay_node_container sh
```



### faucet
To obtain tADA, we need to submit a public key through an HTTP query. This will provide us with 1000 ADA
```bash

# we can generate key pairs with cardano-cli

cardano-cli address key-gen \
    --verification-key-file "key.vkey" \
    --signing-key-file "key.skey" 


# to get tADA we need to provide public key hash
PUBKEYHASHHEX=$(cardano-cli address key-hash --payment-verification-key-file "key.vkey")


# now get ada with query
curl -X POST "congested-testnet.staging.mlabs.city:8000" -H "Content-Type: application/json" -d "{\"pubKeyHashHex\": \"$PUBKEYHASHHEX\"}"
```
this part can be executed using [nix flakes](https://nixos.wiki/wiki/Flakes) inside current repo `nix run .#get-tada`

### submit transactions 
To submit a transaction on the testnet, we can use [ogmios](https://github.com/CardanoSolutions/ogmios) and [kupo](https://github.com/CardanoSolutions/kupo). An example using the [purescript CTL](https://github.com/Plutonomicon/cardano-transaction-lib) can be found [here](./examples/purescript-example/src/Example.purs). Additionally, there are [ogmios clients and tx examples](https://ogmios.dev/clients/) available in other languages. Whichever client you choose to use, simply use next addresses `congested-testnet.staging.mlabs.city:1337` and `congested-testnet.staging.mlabs.city:1442`

### verify transaction
We can verify that the transaction is on the ledger with kupo.
```
curl http://congested-testnet.staging.mlabs.city:1442/matches/*@<transactionHash>
```

### tests 
We can run bats tests using `nix run .#tests`. Additionally, we can monitor Cardano testnet statistics with [prometheus-db](http://congested-testnet.staging.mlabs.city:9090). We can find in Prometheus `await_time_tx` metrics which measure verify transaction time for simple transaction.


### deployment
Run

```bash
nixos-rebuild test --flake .#congested-testnet --target-host root@congested-testnet.staging.mlabs.city
```
