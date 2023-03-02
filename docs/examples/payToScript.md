# Transaction to a script address

This example is taken from the [PPP0303](https://github.com/input-output-hk/plutus-pioneer-program/tree/third-iteration/code/week03)

Building a transaction with a payment to a script address comes with two major changes compared to the [simple transaction](./simpleTx.md). First, instead of a payment key hash, we need a script hash for our address. and second, we need to provide a datum hash (or, since the beginning of the Babagge era, alternatively a `TxOutDatumInline`).

So how do we get script address? Same as for a key address, we can use `cardano-cli address build`, but this time instead of a verification key, we must provide a `Script`.

## Getting the script

coming from the plutus side, we get the `Script` by converting the plutus validator into a cardano-api type and writing it to a file. The function below does this, using the `PlutusScriptSerialised` data constructor which takes as argument a `ShortByteString`

```haskell
writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript
```

Under the hood though, `makeShelleyAddress` (see below) isn't applied to a script, but to a `PaymentCredential`. So the cardano-api provides the `hashScript` function which converts our `PlutusScript PlutusScriptV1 (PlutusScriptSerialised script)` into a value of type `ScriptHash`.
applying the `PaymentCredentialByScript` data constructor to this hashed script finally gives us the correct PaymentCredential.

```haskell
makeShelleyAddress :: NetworkId
                   -> PaymentCredential
                   -> StakeAddressReference
                   -> Address ShelleyAddr
makeShelleyAddress nw pc scr =
    ShelleyAddress
      (toShelleyNetwork nw)
      (toShelleyPaymentCredential pc)
      (toShelleyStakeReference scr)
```

As a last step, the shelley address must be converted into a value of type `AddressInEra` needed to build the txOut for the `txOuts` field of the transaction body content. This can be done with the `shelleyAddressInEra` function. 

## Getting the ScriptData

To build a transaction for a payment to a script, we additionally need the datum, provided to the `cardano-cli` via --tx-out-datum-hash-file in this case.

The cardano-api has its own `data` type. The conversion can be done with the cardano-api function `fromPlutusData :: Plutus.Data -> ScriptData`.

To use this cardano-api data in the cardano-cli, it is serialised to json and written to a file. `scriptDataToJson` works either with `ScriptDataJsonDetailedSchema` (used to write Json into a file) or with `ScriptDataJsonNoSchema` (if inlined in the command directly).

The following code shows how to convert and serialise the unit value (which is an instance of PlutusTx.ToData) and write it to `unit.json`.

```haskell
writeUnit :: IO ()
writeUnit = writeJSON "unit.json" ()

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusTx.toData
```

note: alternatively, we can use the [CardanoAPI module](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-ledger/src/Ledger/Tx/CardanoAPI.hs) from plutus-apps as an interface to the `cardano-api` types, see [hydra-demo](./hydra-demo.md) example.

The unit.json file is then read and parsed into a `TxOutDatumHash ScriptDataInAlonzoEra hash` by the cardano-cli, where hash is of type `Hash ScriptData`. 

## Putting it all together

So, having the address and datum hash, building the TxOut is straightforward (using `ReferenceScriptNone` as `ReferenceScript era` and building a `TxOutValue` with the cardano-api `valueFromList` function for example):

```haskell
data TxOut ctx era = TxOut (AddressInEra    era)
                           (TxOutValue      era)
                           (TxOutDatum ctx  era)
                           (ReferenceScript era)
```

Other then the new `txOut`, the transaction body content is built in exactly the same way as in [simple transaction](./simpleTx). And once we have the TxBodyContent, we can again apply `makeTransactionBodyAutoBalance`, giving us a BalancedTxBody AlonzoEra (for more details on this see [balancing the transaction](./spendFromScript.md#balancing-the-transaction) in the spendFromScript example).

```bash
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1 \
    --change-address $(cat 01.addr) \
    --tx-in abae0d0e19f75938537dc5e33252567ae3b1df1f35aafedd1402b6b9ccb7685a#0 \
    --tx-out "$(cat vesting.addr) 200000000 lovelace" \
    --tx-out-datum-hash-file unit.json \
    --out-file tx.body
```


Also signing and submitting works the same way as in [simple transaction](./simpleTx).

```bash
cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 01.skey \
    --testnet-magic 1 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1 \
    --tx-file tx.signed
```
