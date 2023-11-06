# Congested Testnet design 

## High level concept
- The congested testnet will simulate high loads by sampling spam transactions with characteristics similar to the historical congestion distribution, as detailed in the [statistics](./congestion-statistics.md)
- The testnet will be open to public access.
- A publicly available tADA faucet will be provided on the testnet for developers.
- The testnet will offer a mechanism to submit custom transactions.

## Architechture
![Design](./imgs/design.svg)
##### In our Cardano testnet, some nodes will be integrated with the following APIs:
 - Faucet API. This api will be connected with few testnet nodes annd will be used to send 100 tADA to a specified address.      
     ```bash
    curl -X POST ".../faucet" \
         -H "Content-Type: application/json" \
         -d '{"address": $USER_ADDRESS}'
     ```
 - [BlockFrost API](https://github.com/blockfrost/blockfrost-backend-ryo). This api will allows users to submit transactions to the testnet 
 - Spammer.
    - Spammer continually submits random transactions in round robin manner to each node on the testnet in order to simulate constant congestion. 
    - Transaction parameters such as transaction size, cpu/mem usage will be sampled from [congestion distribution](./congestion-statistics.md) 
    - It will be built on top of [Cardano transaction library(CTL)](https://github.com/Plutonomicon/cardano-transaction-lib) which connects with testnet nodes via [ogmios](https://github.com/CardanoSolutions/ogmios) and [kupo](https://github.com/CardanoSolutions/kupo). 
    - All necessary data for spam transactions, such as random addresses, scripts, and policies, along with parameters from the [congestion distribution](./congestion-statistics.md), will be stored in database (`SpammerDB`).
- Mempool Monitoring. This API requests information from each node in the testnet to estimate the current mempool usage percentage for each node, as well as the average across the testnet.


