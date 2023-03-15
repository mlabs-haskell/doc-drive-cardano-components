# The different keys

chain-wallet from [iog](https://github.com/input-output-hk/chain-wallet-libs/blob/master/doc/CRYPTO.md#master-key-generation-to-cryptographic-key)

normal/extended keys on [stack](https://cardano.stackexchange.com/questions/756/difference-between-normal-key-and-extended-key-in-cardano-cli-address-key-ge)

paymentkey vs stakekey [here](https://developers.cardano.org/docs/stake-pool-course/handbook/keys-addresses/)


what is the difference between `cardano-cli stake-address key-gen` and `cardano-cli address key-gen`
some explanations [here](https://cardano-foundation.gitbook.io/stake-pool-course/stake-pool-guide/stake-pool-operations/keys_and_addresses)

then there is `cardano-cli node ..` for kes and vrf keys, see below

good explanation to extended keys [here](https://river.com/learn/terms/x/xprv-extended-private-key/#:~:text=An%20extended%20private%20key%20is,funds%20associated%20with%20that%20key.)

## what happens in cardano-cli

the different keys, as shown in the `case` distinction below, are PaymentKey, PaymentExtendedKey or ByronKey

```haskell
runAddressKeyGenToFile
  :: AddressKeyType
  -> VerificationKeyFile
  -> SigningKeyFile
  -> ExceptT ShelleyAddressCmdError IO ()
runAddressKeyGenToFile kt vkf skf = case kt of
  AddressKeyShelley         -> generateAndWriteKeyFiles AsPaymentKey          vkf skf
  AddressKeyShelleyExtended -> generateAndWriteKeyFiles AsPaymentExtendedKey  vkf skf
  AddressKeyByron           -> generateAndWriteKeyFiles AsByronKey            vkf skf
```

under the hood, the `cardano-cli` calls:

```haskell
generateKeyPair :: Key keyrole => AsType keyrole -> IO (VerificationKey keyrole, SigningKey keyrole)
generateKeyPair asType = do
  skey <- generateSigningKey asType
  return (getVerificationKey skey, skey)
```

where PaymentKey is an instance of the Key class, and the value of `AsType PaymentKey` is as `AsPaymentKey`.

finally, the key is generated here, where we need the keytype to get the seedSize:

```haskell
generateSigningKey :: Key keyrole => AsType keyrole -> IO (SigningKey keyrole)
generateSigningKey keytype = do
    seed <- Crypto.readSeedFromSystemEntropy seedSize
    return $! deterministicSigningKey keytype seed
  where
    seedSize = deterministicSigningKeySeedSize keytype
```

so, the seedSize depends on the keytype, the function to get the seed then just uses this seedSize. The `deterministicSigningKey` function is defined in the Key class of which the instances are:

in Keys/Shelley
- PaymentKey
- PaymentExtendedKey
- StakeKey           see below, same as PaymentKey
- StakeExtendedKey
- GenesisKey
- GenesisExtendedKey
- GenesisDelegateKey
- GenesisDelegateExtendedKey
- GenesisUTxOKey     this is one/several key/s to spend the funds created in the genesis file [see here](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/shelley-genesis.md)
- StakePoolKey
in Keys/Praos
- KesKey [link](https://github.com/input-output-hk/cardano-node/blob/master/doc/stake-pool-operations/7_KES_period.md), KES stands for Key Evolving Signature
- VrfKey [link](https://medium.com/dcspark/cardano-algorand-leader-selection-explained-81f71a30b8bb), key take away: every stake pool operator has its own VRF key and this VRF key decides whether or not he is elected for a specific slot
in Keys/Byron
- ByronKey
- ByronKeyLegacy


## PaymentKey vs PaymentExtendedKey

If we want to derive child keys like [here](https://input-output-hk.github.io/cardano-wallet/user-guide/cli#key-child), we need a PaymentExtendedKey

see how `cardano-wallet` does this [here](https://github.com/input-output-hk/cardano-wallet/blob/edee0e4ff6edb76908919442ef146d3fcbbfb54e/lib/wallet/src/Cardano/Wallet/Primitive/AddressDerivation/Shelley.hs#L170)
it uses `generateNew` and `XPrv` from [cardano-crypto](https://github.com/input-output-hk/cardano-crypto/blob/develop/src/Cardano/Crypto/Wallet.hs)

!!! cardano-cli also uses the `cardano-crypto` package for extended key, but instead of `generateNew` it uses `generate` (see below Crypto.HD.generate)

### for SigningKey ByronKey:

```haskell
    deterministicSigningKey :: AsType ByronKey -> Crypto.Seed -> SigningKey ByronKey
    deterministicSigningKey AsByronKey seed =
       ByronSigningKey (snd (Crypto.runMonadRandomWithSeed seed Byron.keyGen))
```

explanation from Cardano.Api.Keys.Byron:

Byron-era payment keys. Used for Byron addresses and witnessing
transactions that spend from these addresses.
These use Ed25519 but with a 32byte \"chaincode\" used in HD derivation.
The inclusion of the chaincode is a design mistake but one that cannot
be corrected for the Byron era. The Shelley era 'PaymentKey's do not include
a chaincode. It is safe to use a zero or random chaincode for new Byron keys.

this is runMonadRandomWithSeed in Cardano.Crypto.Seed:

```haskell
-- Support for MonadRandom
--

-- | Run an action in 'MonadRandom' deterministically using a seed as a
-- finite source of randomness. Note that this is not a PRNG, so like with
-- 'getBytesFromSeed' it will fail if more bytes are requested than are
-- available.
--
-- So this is only really suitable for key generation where there is a known
-- upper bound on the amount of entropy that will be requested.
--
runMonadRandomWithSeed :: Seed -> (forall m. MonadRandom m => m a) -> a
runMonadRandomWithSeed s@(Seed bs) a =
    case runIdentity (runMaybeT (evalStateT (unMonadRandomFromSeed a) s)) of
      Just x  -> x
      Nothing -> throw (SeedBytesExhausted (BS.length bs))
```

### for SigningKey PaymentKey:

```haskell
    deterministicSigningKey :: AsType PaymentKey -> Crypto.Seed -> SigningKey PaymentKey
    deterministicSigningKey AsPaymentKey seed =
        PaymentSigningKey (Crypto.genKeyDSIGN seed)
```

### for SigningKey PaymentExtendedKey: 

```haskell
    deterministicSigningKey :: AsType PaymentExtendedKey -> Crypto.Seed -> SigningKey PaymentExtendedKey
    deterministicSigningKey AsPaymentExtendedKey seed =
        PaymentExtendedSigningKey
          (Crypto.HD.generate seedbs BS.empty)
      where
       (seedbs, _) = Crypto.getBytesFromSeedT 32 seed
```
for the last one, we also have a keychain. See explanations above..

### for StakeKey

this one is identical to SigningKey PaymentKey

```haskell
    deterministicSigningKey :: AsType StakeKey -> Crypto.Seed -> SigningKey StakeKey
    deterministicSigningKey AsStakeKey seed =
        StakeSigningKey (Crypto.genKeyDSIGN seed)
```
