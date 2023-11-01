# Congested Testnet design 

## High level concept
- The congested testnet will simulate high loads by sampling spam transactions with characteristics similar to the historical congestion distribution, as detailed in the [statistics](./congestion-statistics.md)
- The testnet will be open to public access.
- A publicly available tADA faucet will be provided on the testnet for developers.
- The testnet will offer a mechanism to submit custom transactions.

## Architechture
![Design](./imgs/design.svg)
##### In our Cardano testnet, some nodes will be integrated with the following APIs:
 - Faucet API. This api sends tADA to the provided address     
 - [BlockFrost API](https://github.com/blockfrost/blockfrost-backend-ryo). This api will allows users to submit transactions to the testnet 
 - Spamer.
    - spamer continually submits random transactions to the testnet in order to simulate constant congestion. 
    - It will be built on top of [Cardano transaction library(CTL)](https://github.com/Plutonomicon/cardano-transaction-lib) which connects with testnet nodes via [ogmios](https://github.com/CardanoSolutions/ogmios) and [kupo](https://github.com/CardanoSolutions/kupo). 
    - all necessary data for spam transactions, such as random addresses, scripts, and policies, along with parameters from the [congestion distribution](./congestion-statistics.md), will be stored in database (`SpamerDB`).

