# cardano-api-documentation

Building, balancing and submitting transactions to the cardano blockchain is usually done with a high level of abstraction, using the [plutus contract monad](https://github.com/input-output-hk/plutus-apps/tree/main/plutus-contract) or the [CTL](https://github.com/Plutonomicon/cardano-transaction-lib) (cardano transaction library) contract monad.

Another tool to build and submit transactions is the `cardano-cli`. the cardano-cli comes with the cardano-node and uses the cardano-api library unter the hood. the following examples, taken from the third iteration of the [plutus pioneer program](https://github.com/input-output-hk/plutus-pioneer-program/tree/third-iteration), try to show how the `cardano-cli` makes use of the `cardano-api` to build, balance, sign and transmit transactions. 

- [simple transaction](./examples/simpleTx.md)
- [pay to script address](./examples/payToScript.md)
- [spend from script address](./examples/spendFromScript.md)
- [minting transaction](./examples/mintTx.md)
- [withraw staking rewards](./examples/withdrawal.md)

Sometimes it may also be necessary to use the cardano-api directly. One example for this is the [mlabs hydra-demo](https://github.com/mlabs-haskell/hydra-demo). The interface to speak to a hydra node is a websocket and so tools like the contract monad and the PAB (plutus application backend) are not (yet) available (see details [here](https://hydra.family/head-protocol/docs/getting-started/developing-on-hydra#off-chain-code)). [hydra-demo](./examples/hydra-demo.md) shows how the cardano-api can be used to build, balance and sign transactions (which then are serialised to `CBOR` and transmitted through websocket to a hydra node).
