# The UTxO in the cardano-api

The Cardano network represents transactions as inputs and outputs of UTxOs (Unspent Transaction Outputs), where the inputs get consumed (if allowed by the provided key/script witnesses) and new outputs get created. A high level explanation of the UTxO model can be found [here](https://developers.cardano.org/docs/stake-pool-course/handbook/utxo-model/#:~:text=An%20unspent%20transaction%20output%20is,cannot%20be%20consumed%20in%20part.)

A little bit more technically, a UTxO can be seen as a data type containing two elements:
- a transaction ID, which is a BLAKE2b-256 hash of the transaction in which it was produced, and an index (because a transaction can of course have multiple outputs)
- its value, the address to which it was sent, since the Alonzo hard fork an optional datum (or datum hash) and, since the Vasil hard fork, an optional reference script (for its usage in connection with reference inputs see [cip33](https://cips.cardano.org/cips/cip33/)).

In the cardano-api, the UTxO is defined as a map of these two elements:

```haskell
newtype UTxO era = UTxO { unUTxO :: Map TxIn (TxOut CtxUTxO era) }
```

where the TxIn contains the transaction ID and index, and the TxOut the remaining data. In json format, obtained for example with the following cardano-cli command:

```bash
cardano-cli query utxo --address ADDR --testnet-magic TESTNET-MAGIC --out-file /dev/stdout
```

```json
{
    "d0a10a69b22ab7ff4877150438d1435587a327be52a0d8066a05fdcbf30787a5#0": {
        "address": "addr_test1vp907jcda78gsxzukt54xynfgzgpq634yvp5836qk6nmkxqv64akd",
        "datum": null,
        "datumhash": null,
        "inlineDatum": null,
        "referenceScript": null,
        "value": {
            "lovelace": 10000000
        }
    }
```

A more interesting example of a UTxO can be found [here](https://github.com/input-output-hk/hydra/blob/master/hydra-node/golden/UTxO'%20(TxOut%20CtxUTxO%20BabbageEra).json).

As the definition and the second example show, a UTxO it is a map of `TxIn`s and `TxOut`s. So being precise, a UTxO is not **one** unspent transaction output, but arbitrarily many. This can lead to confusion, as normaly when speaking about a UTxO, we mean **one** unspent transaction output.

In the cardano-cli, the UTxO data type is used mainly for queries. In these, the cardano-api type `QueryUTxOFilter` is used to filter the query, and with the precise UTxO definition from above in mind, it makes sense that the `WholeUTxO` filter below means querying the chain for all unspent transaction outputs (which is only advisable on small testnets):

```haskell  
data QueryUTxOFilter =
     -- | /O(n) time and space/ for utxo size n
     QueryUTxOWhole
     -- | /O(n) time, O(m) space/ for utxo size n, and address set size m
   | QueryUTxOByAddress (Set AddressAny)
     -- | /O(m log n) time, O(m) space/ for utxo size n, and address set size m
   | QueryUTxOByTxIn (Set TxIn)
```

These filters are used by the `cardano-cli query utxo` command, with the `--address` option for outputs filtered by the provided addresses and the `--tx-in` option for outputs filtered by transaction IDs plus indexes.

When building transactions with the cardano-cli, though, a 'UTxO' is either a `--tx-in` when in the context of a transaction input or a `--tx-out` (plus `--tx-out-datum-hash` or similar options if needed) when in the context of a transaction output. For a transaction output we of course have no other choice, as the transaction hash needed for the TxIn doesn't exist yet at the moment of building the transaction.

As for the transaction inputs: The information contained in the TxIn is obviously not enough to build the transaction. The `cardano-cli` therefore queries the node with above mentioned `QueryUTxOByTxIn` filter inside the `runTxBuild` function. Later in the same function, this UTxO is passed to the cardano-api `makeTransactionBodyAutoBalance` function and converted to its cardano-ledger type to evaluate the transaction execution units.

# The TxOut and its context

Especially noticeable about the TxOut type is that it is tagged with a context, which can be either a transaction (`CtxTx`) or just the UTxO itself (`CtxUTxO`):

```haskell
data TxOut ctx era = TxOut (AddressInEra    era)
                           (TxOutValue      era)
                           (TxOutDatum ctx  era)
                           (ReferenceScript era)
```

The reason for this is the `TxOutDatum` type. When building a transaction with the cardano-cli, depending on the options chosen, the cardan-cli parses the datum information into one of the following values:

```haskell
data TxOutDatumAnyEra = TxOutDatumByHashOnly (Hash ScriptData)
                      | TxOutDatumByHashOf    ScriptDataOrFile
                      | TxOutDatumByValue     ScriptDataOrFile
                      | TxOutInlineDatumByValue ScriptDataOrFile
                      | TxOutDatumByNone
```

After using the datum to build a (cardano-cli) `TxOutAnyEra`, the `runTxBuildCmd` converts this txOut with `toTxOutInAnyEra` into a cardano-api `TxOut CtxTx era` (so, a txOut in the transaction context). 

For the conversion of the cardano-cli `TxOutDatumAnyEra` to the cardano-api `TxOutDatum` inside `toTxOutInAnyEra`, we have three case distinctions: 

- If scripts are not supported (so in all eras before Alonzo), the result is a `TxOutDatumNone`. 
- If scripts are supported but we are not yet in the babbage era, we use `toTxAlonzoDatum`: This can produce the above or a `TxOutDatumHash` or a `TxOutDatumInTx`.
- In Babbage and later, we use `toTxDatumReferenceScriptBabbage`: This can produce all of the above, plus a `TxOutDatumInline` if the cardano-cli input got parsed to a `TxOutInlineDatumByValue` (by using the --tx-out-inline-datum)

The transaction context is important because of the possibilty to produce a `TxOutDatumInTx`:

```haskell
TxOutDatumInTx'  :: ScriptDataSupportedInEra era
                 -> Hash ScriptData
                 -> ScriptData
                 -> TxOutDatum CtxTx era
```

A value of this type contains the non hashed datum (ScriptData). But when we convert a txOut containing such a datum to its ledger type (with `convTxOuts`, to later produce the cardano-ledger transaction body content), we only keep the hash:

```haskell
toAlonzoTxOutDataHash' (TxOutDatumInTx' _ (ScriptDataHash dh) _) = SJust dh
```

The datum itself gets stored in a map in the transaction body (in `TxBodyScriptData`). To get this information using the hash as key, we have `fromLedgerTxOuts`, which internally uses `fromAlonzoTxOutDatum scriptDataInEra datahash`.

So, a `TxOutDatumInTx` makes only sense in the context of a transaction, as we need a transaction body from which we can retrieve the datum. That's why we also have the type `TxOut CtxUTxO era`, that cannot be built with a `TxOutDatumInTx`.

Changing the context of a txOut from transaction to UTxO can be done with the following cardano-api function:

```haskell
toCtxUTxOTxOut :: TxOut CtxTx  era -> TxOut CtxUTxO era
toCtxUTxOTxOut (TxOut addr val d refS) =
  let dat = case d of
              TxOutDatumNone -> TxOutDatumNone
              TxOutDatumHash s h -> TxOutDatumHash s h
              TxOutDatumInTx' s h _ -> TxOutDatumHash s h
              TxOutDatumInline s sd -> TxOutDatumInline s sd
  in TxOut addr val dat refS
```

where the `TxOutDatumInTx` gets converted to a `TxOutDatumHash`, loosing the information about the data and only retaining the hash.

As can be seen in the `UTxO` definition at the beginning of the chapter, the txOuts in the map have to be in the UTxO context. 

Since the start of the Babbage era, the `TxOutDatumInTx` has lost importance though, since a non hashed datum can now be stored in a cardano-ledger txOut. This is done, inside `convTxOuts`, with the following function:

```haskell
toBabbageTxOutDatum' (TxOutDatumInline _ sd) = scriptDataToInlineDatum sd

--where:
scriptDataToInlineDatum :: ScriptData -> Babbage.Datum ledgerera
scriptDataToInlineDatum = Babbage.Datum . Alonzo.dataToBinaryData . toAlonzoData
```
