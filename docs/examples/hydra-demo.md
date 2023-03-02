# building transactions by hand

to construct a cardano-api `TxBody` we need first the bodyContent. We will construct a cardano-api `TxBodyContent BuildTx AlonzoEra`, for which we need, in particular, the two fields `txIns` and `txOuts`

## constructing the fitting txIns

The `txIns` field expects a value of the following type:

`type TxIns build era = [(TxIn, BuildTxWith build (Witness WitCtxTxIn era))]`

so, next to the TxIn itself, there is the information of whether the TxIn needs a `KeyWitness` or a `ScriptWitness`

```haskell
-- for spending with a KeyWitness
txInForSpending :: TxIn -> (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))
txInForSpending = (,BuildTxWith (KeyWitness KeyWitnessForSpending))

-- for spending with a ScriptWitness
txInForValidator ::
  (PlutusTx.ToData d, PlutusTx.ToData r) =>
  TxIn ->
  Validator ->
  TxDatum d ->
  TxRedeemer r ->
  ExecutionUnits ->
  Either ToCardanoError (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra))
txInForValidator txIn validator (TxDatum datum) (TxRedeemer redeemer) exUnits = do
  scriptInEra <- toCardanoScriptInEra (getValidator validator)
  case scriptInEra of
    ScriptInEra lang (PlutusScript version script) ->
      pure
        ( txIn
        , BuildTxWith $
            ScriptWitness ScriptWitnessForSpending $
              PlutusScriptWitness
                lang
                version
                script
                (ScriptDatumForTxIn (toCardanoData datum))
                (toCardanoData redeemer)
                exUnits
        )
    -- only Plutus scripts are supported
    ScriptInEra _ (SimpleScript _ _) -> Left DeserialisationError
```

important: `toCardanoScriptInEra` and `toCardanoAPIData` (which is used by `toCardanoData`) are functions from the `plutus-apps` module `Ledger.Tx.CardanoApi.Internal`. This module is an interface to the transaction types from 'cardano-api' and gets re-exported through [Ledger.Tx.CardanoAPI](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-ledger/src/Ledger/Tx/CardanoAPI.hs)

## constructing the fitting txOuts

```haskell
data TxOut ctx era = TxOut (AddressInEra    era)
                           (TxOutValue      era)
                           (TxOutDatum ctx  era)
                           (ReferenceScript era)
```

### TxOut to key address

Constructing a TxOut to a key address is pretty straightforward. For the address, we first we need a `SigningKey PaymentKey` which we get (as `PaymentKey` has an instance of the `Key` class) with:

`generateSigningKey :: Key keyrole => AsType keyrole -> IO (SigningKey keyrole)`

The corresponding `VerificationKey PaymentKey` and then its hash is obtained with:

`vkeyHash = verificationKeyHash (getVerificationKey skey)`, with both of these functions defined in the `Key` class.

Defining the networkId as either `Mainnet` or `Testnet NetworkMagic`, we get the address with another cardano-api function:

`keyAddr = makeShelleyAddressInEra networkId (PaymentCredentialByKey vkeyHash) NoStakeAddress`

next to the address, we need a value. For this, cardano-api provides the function `lovelaceToTxOutValue` which we apply to some `Lovelace` (a newtype wrapper around `Integer`).
The datum field of the `TxOut` is `TxOutDatumNone`, as this is a payment to a key address.

putting it all together:

`addressOut = TxOut keyAddr (lovelaceToTxOutValue lovelace) TxOutDatumNone ReferenceScriptNone`

### TxOut to script address

To construct a TxOut for a script address we additionally need a `TxOutDatumHash`. if the datum d is an instance to the `ToData` class, we can convert it as follows:

`datumHash = TxOutDatumHash ScriptDataInAlonzoEra (hashScriptData $ toCardanoData d)`

where `toCardanoData` again is from Ledger.Tx.CardanoAPI.

For the script address, the `PaymentCredential` must be a `PaymentCredentialByScript ScriptHash`, having this we can again construct the address with

`scriptAddr = makeShelleyAddressInEra networkId (PaymentCredentialByScript scriptHash) NoStakeAddress`

And the resulting TxOut is then:

`scriptOut = TxOut scriptAddr (lovelaceToTxOutValue lovelace) datumHash ReferenceScriptNone`

## Putting it all together

So, putting it all together in a real world example:

```haskell
-- simplified, from `buildBetTx`, having a validatorAddress and a changeAddress
-- this tx only spends from key addresses, so the txIns are built with `txInForSpending`
-- this tx pays to a script (scriptOut) and to a key address (addressOut)
buildTxBody :: [TxIn] -> Either String (TxBody AlonzoEra)
buildTxBody inputRefs = do
  let bodyContent =
        baseBodyContent
          { txIns = txInForSpending <$> inputRefs
          , txOuts = scriptOut : addressOut
          }
  first (("bad tx-body: " <>) . show) $ createAndValidateTransactionBody bodyContent
```

where `createAndValidateTransactionBody` (respectively the deprecated `makeTransactionBody` which is originally used in the example) is taken from the cardano-api. And as `baseBodyContent` we have:

```haskell
baseBodyContent :: TxBodyContent BuildTx AlonzoEra
baseBodyContent =
  TxBodyContent
    { txIns = []
    , txInsCollateral = TxInsCollateralNone
    , txOuts = []
    , txFee = TxFeeExplicit TxFeesExplicitInAlonzoEra 0
    , txValidityRange = (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra)
    , txMetadata = TxMetadataNone
    , txAuxScripts = TxAuxScriptsNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith Nothing
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    , txScriptValidity = TxScriptValidity TxScriptValiditySupportedInAlonzoEra ScriptValid
    }
```

note: as we see from the TxBodyContent, we are in the AlonzoEra, this is decisive for `createAndValidateTransactionBody` and particularly for `makeShelleyTransactionBody`, which depending on the era builds a different TxBody
in our case, we have the `ShelleyBasedEraAlonzo`, (must be ShelleyBased as in the Byron era there were no complex transactions). so our baseBodyContent is missing the fields 

- txInsReference =  TxInsReferenceNone        :: TxInsReference build era,
- txTotalCollateral = TxTotalCollateralNone   :: TxTotalCollateral era,
- txReturnCollateral = TxReturnCollateralNone :: TxReturnCollateral CtxTx era,

of course we could add them and build a `TxBody BabbageEra`

note: the `txFee` is set to 0 at this point because this transaction will be sent to a hydra head via websocket and thus fee rules for the cardano mainnet/testnets don't apply. To build a transaction that is to be sent to a cardano-node, we would use `makeTransactionBodyAutoBalance` instead of `createAndValidateTransactionBody`. `makeTransactionBodyAutoBalance` needs more information, though, all of which can be queried from a local node. how this is done is shown in the `runTxBuildCmd` function from the [Cardano.CLI.Shelley.Run.Transaction](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/src/Cardano/CLI/Shelley/Run/Transaction.hs) module

## Transaction that spends from script address

To build a transaction that spends from a script address, see `buildClaimTx` from the [hydra-demo](https://github.com/mlabs-haskell/hydra-demo/blob/master/src/HydraRPS/App.hs). 

There is one main difference to the example above: The `txIns` field of the TxBodyContent contains elements (see `myValidatorTxIn` and `theirValidatorTxIn` below) that are built with `txInForValidator` instead of `txInForSpending`. So, instead of being paired with a `keyWitness`, their `txIn` is paired with a `scriptWitness`.

```haskell
let bodyContent =
      baseBodyContent
        { txIns = [myValidatorTxIn, theirValidatorIxIn]
        , txInsCollateral = TxInsCollateral CollateralInAlonzoEra [collateralTxIn]
        , txOuts = outputs
        , txProtocolParams = BuildTxWith (Just state.hsProtocolParams)
        }
```

note: as this transaction is not going to be sent to a cardano node but to a hydra head, the execution units defined in the script witnesses are just set to the half of the maxTxExUnits: 

```haskell
ExecutionUnits
  { executionSteps = executionSteps maxTxExUnits `div` 2
  , executionMemory = executionMemory maxTxExUnits `div` 2
  }
```

In the case of a transaction for the cardano blockchain, the evaluation of the execution units is done with `evaluateTransactionExecutionUnits` inside the `makeTransactionBodyAutoBalance` function, where all the scripts are run to count the execution units.

## Sign and submit the transaction

Having a balanced transaction of type `TxBody AlonzoEra`, signing this unsigned transaction is straightforward:

`signedTx = signTx userSkey unsignedTx`

```haskell
signTx :: SigningKey PaymentKey -> TxBody AlonzoEra -> Tx AlonzoEra
signTx signingKey body = Tx body [witness]
  where
    witness = makeShelleyKeyWitness body (WitnessPaymentKey signingKey)

-- this is possible because of the following pattern declaration
pattern Tx :: TxBody era -> [KeyWitness era] -> Tx era
pattern Tx txbody ws <- (getTxBodyAndWitnesses -> (txbody, ws))
  where
    Tx txbody ws = makeSignedTransaction ws txbody
```

Submitting the transaction is done through the websocket. To submit a transaction to a cardano-node see the other examples.
