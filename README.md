# congested-testnet
The goal of this project is to operate a cardano testnet under constant congestion. Therefore, an additional component, a spammer, is included. This component continuously generates transactions to simulate periods of congestion commonly observed on the mainnet.

- [Congestion research](./docs/congestion-statistics.md)
- [Design](./docs/design.md)
- [How to use](#how-to-use)

## how to use
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
We can run bats tests using `nix run .#tests`. Additionally, we can monitor Cardano testnet statistics with [prometheus-db](http://congested-testnet.staging.mlabs.city:9090) \
We have wrapper for the cardano-cli. You can query it like this:
```
curl -X POST http://congested-testnet.staging.mlabs.city:8001/cli  -H "Content-Type: application/json"  -d '{"args": "query slot-number --testnet-magic 2 2024-03-31T15:16:27Z"}'
```
This command shows the slot number for the current testnet at the specified timestamp

### deployment
Run

```bash
nixos-rebuild test --flake .#congested-testnet --target-host root@congested-testnet.staging.mlabs.city
```
