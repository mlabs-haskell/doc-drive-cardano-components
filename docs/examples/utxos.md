# from utxo to txIn and txOut

what is an utxo? the cardano-api knows this term only in connection to a query.

So, `QueryUTxOFilter` provides various ways to query a filtered subset of the UTxO, where `UTxO` means all existing utxos on the blockchain.

Also: in the context of a minimum utxo value, whereby this resembles more a minimum txOut value, as the function `calculateMinimumUTxO` takes as an argument a 
-> TxOut CtxTx era

and thirdly: we use CtxUTxO vs CtxTx to indicate whether the `TxOutDatum` of TxOut can be a `TxOutDatumInTx` (in the case of CtxTx) or not (in the case of CtxUTxO). (To go from one to the other we have toCtxUTxOTxOut)

explanation Cardano.Api.TxBody:

A transaction output that specifies the whole datum value. This can
only be used in the context of the transaction body, and does not occur
in the UTxO. The UTxO only contains the datum hash.

also, for estimating execution units and so forth, where we need functions from cardano-ledger, we use cardano-api utxo and convert it into Ledger.UTxO, see `toLedgerUTxO` in `evaluateTrasactionExecutionUnits` in Cardano.Api.Fees

so, there really are just TxIn and TxOut

In the context of building a transaction, the utxo that we produce is a TxIn and the utxo that we produce is a TxOut.
For TxOut that's obvious, as we don't have the TxId and the TxIx yet.
For the TxIn it's a little bit more complicated. The `cardano-cli` expects a TxIn, but under the hood it builds a (txIn, witness)

So: you can 
utxo is a very confusing term, in `cardano-cli` it basically means all unspent transaction outputs, their TxIx (hash), TxId (id), Value, Address, Datum, and ReferenceScript. for an example of such a map in json format, see:

[utxo.json](https://github.com/input-output-hk/hydra/blob/master/hydra-node/golden/UTxO'%20(TxOut%20CtxUTxO%20BabbageEra).json)


from cardano-hydra-api/Cardano.Api.UTxO:

This module is name-spaces slightly different from the rest
because it is meant to be used as a replacement of the UTxO type of the
cardano-api which is not convenient enough to work with. Having it as
'Hydra.Cardano.Api.UTxO' causes cyclic imports with other modules also
relying on this newtype. So instead, we do 'as if' it was part of the
cardano-api in the first palce.

in Cardano.Api.Query, UTxO is defined as:

```haskell
newtype UTxO era = UTxO { unUTxO :: Map TxIn (TxOut CtxUTxO era) }
  deriving (Eq, Show)
```

whereas in ``hydra`` Cardano.Api.UTxO, it is defined as:

```haskell
type Era = BabbageEra

type UTxO = UTxO' (TxOut CtxUTxO Era)

-- | Newtype with phantom types mostly required to work around the poor interface
-- of 'Ledger.UTXO' and provide 'Monoid' and 'Foldable' instances to make utxo
-- manipulation bareable.
newtype UTxO' out = UTxO
  { toMap :: Map TxIn out
  }
  deriving newtype
    ( Eq
    , Show
    , Functor
    , Foldable
    , Semigroup
    , Monoid
    )
```
what could be the reason? Maybe, that the Ledger.TxOut can either be a ByronTxOut, ShelleyTxOut or BabbageTxOut

The problem with the ledger utxo (as explained in Hydra.Cardano.Api.Value):

```haskell
-- | Calculate minimum value for a UTxO. Note that cardano-api defines a
-- 'calculateMinimumUTxO' function but it is flawed (see NOTE below) and has an
-- unsatisfactory API because it works across multiple eras.
--
-- This one is specialized to Babbage and therefore, can be pure.
minUTxOValue ::
  ProtocolParameters ->
  TxOut CtxTx Era ->
  Value
```

the cardano-api `calculateMinimumUTxO`? very complicated, works across multiple eras.., and before Alonzo era, this information is in the protocol parameters

problem: for `evaluateMinLovelaceOutput` inside, we need a `Ledger.TxOut ledgerera`, which can either be a `ByronTxOut`, a `ShelleyTxOut` or a `BabbageTxOut`.


what happens when I use `cardano-cli query utxo`, filtered by address? it produces a list of TxIn belonging to this address, plus the value, the datum (if present), so it is the subset of the utxo 
