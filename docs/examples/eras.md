# Cardano and its eras

When talking about (ledger) eras in Cardano, it's important not to confuse them with development phases such as Shelley (used as a name for both a ledger era and a development phase), Goguen or Basho. The difference between the two concepts is explained [here](https://docs.cardano.org/explore-cardano/eras-and-phases). In short: The development phases focus on high level topics (Shelley on decentralisation, for example, or Basho on scalability), whereas ledger eras define a concrete collection of ledger features (what transaction types and what data is stored in the ledger for example) introduced with a hard fork.

When talking about eras in the context of the cardano-api, we are meaning the ledger eras. At the moment of writing, there are six such eras, defined in the [Cardano.Api.Eras](https://github.com/input-output-hk/cardano-node/blob/master/cardano-api/src/Cardano/Api/Eras.hs) module. The latest era is the Conway era which brought more interoparability with other chains like Bitcoin or Ethereum by adding support for the SECP256k1 cryptographic system.

In the cardano-api, the era types are used to tag other types like `TxBody` or `TxOut`, assuring that components from different eras don't get mixed together. Then, when we build transactions, these era type tags get at some point mapped to the corresponding type tags used in the ledger library. The mapping for the Shelley era and onwards (so all except the Byron era) is defined by the `type family ShelleyLedgerEra era` in Cardano.Api.Eras.

As an example of such a mapping, we can take the `toShelleyTxOut` conversion function:

```haskell
toShelleyTxOut :: forall era ledgerera.
                  ShelleyLedgerEra era ~ ledgerera
               => ShelleyBasedEra era
               -> TxOut CtxUTxO era
               -> Ledger.TxOut ledgerera
```

Let's say we have built a `TxOut` for the `AlonzoEra`. Then, given the equivalence constraint between `ShelleyLedgerEra AlonzoEra` and `ledgerera`, the resulting `Ledger.TxOut` through pattern matching will also be in the Alonzo era: 

```haskell
toShelleyTxOut _ (TxOut addr (TxOutValue MultiAssetInAlonzoEra value) txoutdata _) =
    AlonzoTxOut (toShelleyAddr addr) (toMaryValue value) (toAlonzoTxOutDataHash txoutdata)
```

The `Ledger.TxOut` type is defined in the `EraTxOut` class in [Cardano.Ledger.Core](https://github.com/input-output-hk/cardano-ledger/blob/master/libs/cardano-ledger-core/src/Cardano/Ledger/Core.hs) and the class instance for the Alonzo era, the `AlonzoTxOut` data constructor, in [Cardano.Ledger.Alonzo.TxOut](https://github.com/input-output-hk/cardano-ledger/blob/master/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/TxOut.hs) 

## Shelley-based eras

As seen above with `ShelleyLedgerEra`, the cardano-api makes a strong distinction between Byron (the first era) and Shelley (the other five eras starting with the Shelley era).
The reason for this, as explained in `Cardano.Ledger.Eras`, is that the latter are all eras that are based on Shelley with only minor differences, so it is useful to be able to treat the Shelley-based eras in a mostly-uniform way.

Take for example `validateTxBodyContent` from Cardano.Api.TxBody. As the `TxBodyContent` of a Byron transaction is very simple and doesn't need validation (see how to generate a Byron transaction with [txSpendUTxOByronPBFT](https://github.com/input-output-hk/cardano-node/blob/74dc39653653ef41fdf437dabedd580e647db67c/cardano-cli/src/Cardano/CLI/Byron/Tx.hs#L186), this function only accepts transaction body contents from the Shelley era and later.

```haskell
validateTxBodyContent
  :: ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> Either TxBodyError ()
```

The `era` cannot be `ByronEra`, as the GADT below doesn't permit to construct a value with the type variable `ByronEra`

```haskell
data ShelleyBasedEra era where
     ShelleyBasedEraShelley :: ShelleyBasedEra ShelleyEra
     ShelleyBasedEraAllegra :: ShelleyBasedEra AllegraEra
     ShelleyBasedEraMary    :: ShelleyBasedEra MaryEra
     ShelleyBasedEraAlonzo  :: ShelleyBasedEra AlonzoEra
     ShelleyBasedEraBabbage :: ShelleyBasedEra BabbageEra
```

The exact `era` has still its significance inside the `validateTxBodyContent` function, though, as it determines which validation checks are run.

If we are in the `BabbageEra` (respectively in the `ShelleyBasedEraBabbage`) for example, the function validates the following data:

```haskell
       ShelleyBasedEraBabbage -> do
         validateTxIns txIns
         validateTxOuts era txOuts
         validateMetadata txMetadata
         validateMintValue txMintValue
         validateTxInsCollateral txInsCollateral languages
         validateProtocolParameters txProtocolParams languages
```

whereas if we are in the `ShelleyEra` (respectively in the `ShelleyBasedEraShelley`), there is only the txIns, txOuts and the txMetadata to validate:

```haskell
       ShelleyBasedEraShelley -> do
         validateTxIns txIns
         validateTxOuts era txOuts
         validateMetadata txMetadata
```

The era information is not needed everywhere, of course. Take for example the cardano-api type `ScriptHash`. By applying `hashScript` to a plutus script (V1 or V2), we get an era independent `ScriptHash`:

```haskell
hashScript :: Script lang -> ScriptHash
```

Having this, we can apply the data constructor `PaymentCredentialByScript` to get a (still era independent) `PaymentCredential` which can then be used to construct a `Address ShelleyAddress`:

```haskell
makeShelleyAddress :: NetworkId
                   -> PaymentCredential
                   -> StakeAddressReference
                   -> Address ShelleyAddr
```

If we want to use this address in a type that will get converted to its corresponding ledger type, we need to add the era information, though. So, to build the `TxOut` used in `toShelleyTxOut` (see above), we need to tag our Shelley address with a specific era:

```haskell
shelleyAddressInEra :: IsShelleyBasedEra era
                    => Address ShelleyAddr -> AddressInEra era
shelleyAddressInEra = AddressInEra (ShelleyAddressInEra shelleyBasedEra)

-- that can then be used here
data TxOut ctx era = TxOut (AddressInEra    era)
                           (TxOutValue      era)
                           (TxOutDatum ctx  era)
                           (ReferenceScript era)
```

## Working with pattern synonyms

Given that cardano-ledger needs the era information to build the components of a transaction and the transaction itself, we cannot omit them in the cardano-api, unfortunately. But there is a way to get rid of the era tags, as the [hydra-cardano-api](https://github.com/input-output-hk/hydra/tree/master/hydra-cardano-api) demonstrates, by defaulting all types to the latest era (which should be Conway era but is Babbage era, as the code is not updated yet).

`hydra-cardano-api` accomplishes this by using pattern synonyms. The example below shows how the `cardano-api` type `TxOut` gets simplified. First, a new type `TxOut ctx` is defined, that wraps the cardano-api type `TxOut ctx Era` (with `type Era = BabbageEra` coming from `Hydra.Cardano.Api.Prelude`). To construct a value of this type and to use it in pattern matches, a biderictional pattern synonym is defined in the following way:

```haskell
-- using type Era = BabbageEra
type TxOut ctx = Cardano.Api.TxOut ctx Era
{-# COMPLETE TxOut #-}

-- | TxOut specialized for 'Era'
pattern TxOut :: AddressInEra -> Value -> TxOutDatum ctx -> ReferenceScript -> TxOut ctx
pattern TxOut{txOutAddress, txOutValue, txOutDatum, txOutReferenceScript} <-
  Cardano.Api.TxOut
    txOutAddress
    (TxOutValue MultiAssetInBabbageEra txOutValue)
    txOutDatum
    txOutReferenceScript
  where
    TxOut addr value datum ref =
      Cardano.Api.TxOut
        addr
        (TxOutValue MultiAssetInBabbageEra value)
        datum
        ref
```

Note that the arguments for the simplified `TxOut` are themselves simplified with pattern synonyms. The `AddressInEra`, for example, is defined like this:

```haskell
type AddressInEra = Cardano.Api.AddressInEra Era
{-# COMPLETE ShelleyAddressInEra, ByronAddressInEra #-}

pattern ShelleyAddressInEra :: Address ShelleyAddr -> AddressInEra
pattern ShelleyAddressInEra{address} <-
  Cardano.Api.AddressInEra Cardano.Api.ShelleyAddressInEra{} address
  where
    ShelleyAddressInEra =
      Cardano.Api.AddressInEra ShelleyAddressInAnyEra

pattern ByronAddressInEra :: Address ByronAddr -> AddressInEra
pattern ByronAddressInEra{byronAddress} <-
  Cardano.Api.AddressInEra Cardano.Api.ByronAddressInAnyEra byronAddress
  where
    ByronAddressInEra =
      Cardano.Api.AddressInEra ByronAddressInAnyEra
```

The hydra-cardano-api [readme](https://github.com/input-output-hk/hydra/blob/master/hydra-cardano-api/README.md) shows how much less code is needed to build such a `TxOut` with the help of the pattern synonym. 

This is how it is built with the standard `cardano-api`: 

```haskell
changeOutput :: Address ShelleyAddr -> TxOut CtxUTxO BabbageEra
changeOutput =
  TxOut
    (AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) addr)
    (TxOutValue MultiAssetInBabbageEra (lovelaceToValue $ initialAmount - amount - fee))
    (TxOutDatumHash ScriptDataInBabbageEra (hashScriptData $ fromPlutusData someDatum))
```

and this is how the function looks using the wrapped API:

```haskell
changeOutput :: Address ShelleyAddr -> TxOut CtxUTxO
changeOutput =
  TxOut
    (AddressInEra addr)
    (lovelaceToValue $ initialAmount - amount - fee)
    (TxOutDatumHash $ hashScriptData $ fromPlutusData someDatum)
```

There are plans to merge hydra-cardano-api into cardano-api at some point, though if and when is unclear at the moment.
