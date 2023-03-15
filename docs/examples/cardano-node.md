# install cardano-node and cardano-cli

use this [guide](https://developers.cardano.org/docs/get-started/installing-cardano-node)

error when running the node: [stack tip](https://cardano.stackexchange.com/questions/9202/unable-to-run-cardano-node-on-preview-and-pre-production-testnet)

getting the config files [here](https://book.world.dev.cardano.org/environments.html)


do the transaction, from ppp0303 [here](https://github.com/input-output-hk/plutus-pioneer-program/blob/third-iteration/code/week03/testnet/send.sh)

note: it doesnt matter if I use a PaymentKey or a PaymentKeyExtended. In the case of an extended key, it is cast to a normal key with `castVerificationKey`, see for example `runAddressBuild` in cardano-cli