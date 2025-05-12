# congested-testnet
The goal of this project is to operate a cardano testnet under constant congestion. To achieve this, an additional *spammer* component has been added. This component continuously generates transactions to simulate periods of congestion commonly observed on the mainnet.

- [Congestion research](./docs/congestion-statistics.md)
- [Design](./docs/design.md)
- [How to use](#how-to-use)

## How to Use

This README guides you through setting up and running your own congested testnet locally. You'll be able to run a Genesis SPO node with a spammer to simulate network congestion.

### Prerequisites

To run the congested testnet, you'll need:

- `docker` and `docker-compose` installed
- `nodejs` (optional, for running the example)
- `cardano-node` and `cardano-cli` (can be run via Docker)

## Running the Congested Testnet

You can run your own congested testnet locally by running a spo node with Ogmios, Kupo, Faucet, and simulate congestion using the Spammer component. The congestion level can be regulated using the `MEMPOOL_PAUSE_LIMIT` (max 1,000,000 bytes), which means the Spammer will run until the mempool reaches the target value.

If the cardano-node on your machine outpaces the Spammer and you want to simulate higher congestion, you can reduce the block size, increase the slotLength, or both.

```bash
git clone https://github.com/mlabs-haskell/congested-testnet
cd congested-testnet
MEMPOOL_PAUSE_LIMIT=200000 \
SLOT_LENGTH=1 \
MAX_BLOCK_BODY_SIZE=65000 \
SPAMMER_ON=true \
FAUCET_ON=true \
docker-compose --profile genesis_spo up -d
```

This will start:
- A Genesis SPO node
- Ogmios on port `1337`
- Kupo on port `1442`
- Faucet on port `8000`
- Config sharing service on port `5000`
- Prometheus metrics on port `9090`


### Using the Faucet

To obtain tADA, you can submit a public key through an HTTP query to your local faucet. This will provide you with 1000 ADA.

```bash
# Generate key pairs with cardano-cli
cardano-cli address key-gen \
    --verification-key-file "key.vkey" \
    --signing-key-file "key.skey" 

# Get the public key hash
PUBKEYHASHHEX=$(cardano-cli address key-hash --payment-verification-key-file "key.vkey")

# Request tADA from the faucet
curl -X POST "localhost:8000" -H "Content-Type: application/json" -d "{\"pubKeyHashHex\": \"$PUBKEYHASHHEX\"}"
```

### Submitting Transactions

To submit a transaction on your local testnet, you can use [ogmios](https://github.com/CardanoSolutions/ogmios) and [kupo](https://github.com/CardanoSolutions/kupo). The process is demonstrated in the [`js-example`](./examples/js-example/get-ada-submit-tx.js), which shows:

1. Withdrawing funds from the **Faucet**
2. Submitting a tx through **Ogmios**
3. Checking when the tx appears on chain with **Kupo**

To run the JS example (you may need to modify it to point to your local services):

```bash
cd examples/js-example
npm install
node .
```

You can use cardano-cli with HTTP requests as shown in the example, or use offchain libraries like [purescript CTL](https://github.com/Plutonomicon/cardano-transaction-lib) or [ogmios clients](https://ogmios.dev/clients/) available in other languages.

Connect to your local services at:
- `localhost:1337` for Ogmios
- `localhost:1442` for Kupo

### Verifying Transactions

You can verify that a transaction is on the ledger with kupo:

```bash
curl http://localhost:1442/matches/*@<transactionHash>
```

### Running Tests

You can run bats tests using `nix run .#tests`. Additionally, you can monitor your local Cardano testnet statistics with Prometheus at `http://localhost:9090`. The `await_time_tx` metric in Prometheus measures verification time for simple transactions.

### Docker Helper Commands

For convenience, you can set up aliases for cardano-node and cardano-cli:

```bash
alias cardano-node="docker run --rm -it -v $(pwd):/workspace ghcr.io/intersectmbo/cardano-node:10.4.1 /bin/cardano-node"
alias cardano-cli="docker run --rm -it -v $(pwd):/workspace ghcr.io/intersectmbo/cardano-node:10.4.1 /bin/cardano-cli"
```
