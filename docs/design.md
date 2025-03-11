# Congested Testnet design 

## High level concept
- The congested testnet will simulate high loads by sampling spam transactions with characteristics similar to the historical congestion distribution, as detailed in the [statistics](./congestion-statistics.md)
- The testnet will be open to public access.
- A publicly available tADA faucet will be provided on the testnet for developers.
- The testnet will offer a mechanism to submit custom transactions.

## Architechture
<p align="center">
  <img src="./imgs/design.svg"/>
</p>

##### In our Cardano testnet, some nodes will be integrated with the following APIs:
 - Faucet. This api will be connected with few testnet nodes annd will be used to send 1000 tADA to a specified address.      
     ```bash
    curl -X POST ".../faucet" \
         -H "Content-Type: application/json" \
         -d '{"address": $USER_ADDRESS}'
     ```
 - [Prometheus](https://prometheus.io/).This database stores logs from nodes in the testnet. Additionally, the spammer continuously measures the transaction time until it is added to a block `awaitTxTime` . 
 - Spammer.   
    - Uses N wallets, switching between them in a round-robin manner after each transaction. 
    - Transactions can be simple or involve locking/unlocking funds to the alwaysTrueScript with different parameters to simulate congestion, similar to the [historical peak](./congestion-statistics.md) 
    - The size of the transaction and its CPU/memory usage will be regulated by alwaysTrue validators performing varying computational tasks.
    - Transaction parameters such as transaction size, cpu/mem usage will be sampled from [congestion distribution](./congestion-statistics.md) 
    - It will be built on top of [Cardano transaction library(CTL)](https://github.com/Plutonomicon/cardano-transaction-lib) which connects with testnet nodes via [ogmios](https://github.com/CardanoSolutions/ogmios) and [kupo](https://github.com/CardanoSolutions/kupo). 
    - All necessary data for spam transactions, such as random addresses, scripts, and policies, along with parameters from the [congestion distribution](./congestion-statistics.md), will be stored in database (`SpammerDB`).
    - Spammer will query `Prometheus` to check whether to continue generating spam or to stop.
        


