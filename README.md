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

To follow these examples, ensure you have `nodejs` and `docker` installed. 

```bash
alias cardano-node="docker run --rm -it -v $(pwd):/workspace ghcr.io/intersectmbo/cardano-node:10.4.1 /bin/cardano-node"
alias cardano-cli="docker run --rm -it -v $(pwd):/workspace ghcr.io/intersectmbo/cardano-node:10.4.1 /bin/cardano-cli"
npm i node-fetch
node examples/get-ada-submit-tx.js

```  

You can also run your own Genesis SPO node with Ogmios, Kupo, Faucet, Share Config, and Prometheus Metrics, and simulate congestion using Spammer. It is worth mentioning that we can regulate congestion using the MEMPOOL_PAUSE_LIMIT (max 1_000_000 bytes). This means, the Spammer will run until the mempool reaches the target value.

It is possible that the cardano-node on your machine outpaces the Spammer, and you want to simulate higher congestion. In this case, we can reduce the block size, increase the slotLength, or both.

```bash
git clone https://github.com/mlabs-haskell/congested-testnet
cd congested-testnet
MEMPOOL_PAUSE_LIMIT=200000 \
SLOT_LENGTH=2 \
MAX_BLOCK_BODY_SIZE=20000 \
SPAMMER_ON=true \
FAUCET_ON=true \
docker-compose --profile genesis_spo up -d
```
If you only need to run a relay node connected to the Genesis SPO, use the following command:
```bash
SPO_ADDRESS=http://congested-testnet.staging.mlabs.city docker-compose --profile relay_node up -d
```
All configs are available on `http://congested-testnet.staging.mlabs.city:5000`. So you can run your own node  



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
To submit a transaction on the testnet, we can use [ogmios](https://github.com/CardanoSolutions/ogmios) and [kupo](https://github.com/CardanoSolutions/kupo). For this, you can use cardano-cli with http requests, like in [`cardano-cli-nodejs` example](./examples/get-ada-submit-tx.js). Additionally there are offchain libraries like [purescript CTL](https://github.com/Plutonomicon/cardano-transaction-lib), [ogmios clients and tx examples](https://ogmios.dev/clients/) available in other languages. Whichever client you choose to use, simply use next addresses `congested-testnet.staging.mlabs.city:1337` and `congested-testnet.staging.mlabs.city:1442`

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
