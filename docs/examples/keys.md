# The different keys

Cardano uses the the public-key signature system [Ed25519](https://en.wikipedia.org/wiki/EdDSA). There is a wide variety of private/public key pairs (resp. `SigningKey`/`VerificationKey`s in cardano-api terms). High level information about the different keys can be found [here](https://developers.cardano.org/docs/operate-a-stake-pool/cardano-key-pairs/), while the interface to generate and serialise them is defined in the `Key` class in [Cardano.Api.Keys.Class](https://github.com/input-output-hk/cardano-node/blob/master/cardano-api/src/Cardano/Api/Keys/Class.hs). The class instances for the different keys (called `keyrole`s in the cardano-api) are in the following modules:

- Cardano.Api.Keys.Shelley
  - PaymentKey
  - PaymentExtendedKey
  - StakeKey
  - StakeExtendedKey
  - GenesisKey
  - GenesisExtendedKey
  - GenesisDelegateKey
  - GenesisDelegateExtendedKey
  - GenesisUTxOKey, this is one/several key/s to spend the funds created in the genesis file [see here](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/shelley-genesis.md)
  - StakePoolKey, this is a very sensitive key, [see here](https://developers.cardano.org/docs/operate-a-stake-pool/cardano-key-pairs/#cardano-stake-pool-key-pairs)
- Cardano.Api.Keys.Praos
  - KesKey [link](https://github.com/input-output-hk/cardano-node/blob/master/doc/stake-pool-operations/7_KES_period.md), KES stands for Key Evolving Signature
  - VrfKey [link](https://medium.com/dcspark/cardano-algorand-leader-selection-explained-81f71a30b8bb), key take away: every stake pool operator has its own VRF key and this VRF key decides whether or not he is elected for a specific slot
- Cardano.Api.Keys.Byron
  - ByronKey
  - ByronKeyLegacy

So, having a `keyrole` that is an instance of the `Key` class, we can generate the signing key using the following function:

```haskell
generateSigningKey :: Key keyrole => AsType keyrole -> IO (SigningKey keyrole)
generateSigningKey keytype = do
    seed <- Crypto.readSeedFromSystemEntropy seedSize
    return $! deterministicSigningKey keytype seed
  where
    seedSize = deterministicSigningKeySeedSize keytype
```

where the `deterministicSigningKey` and `deterministicSigningKeySeedSize` functions are coming from the `Key` interface.

Maybe the most prominent `keyrole` on Cardano is the PaymentKey. Its `SigningKey PaymentKey` is used to sign transactions that spend funds locked at a key address (i.e an address whose payment credential is a hashed `VerificationKey PaymentKey` as opposed to a script address whose payment credential is a hashed script).

So, let's take the keytype `AsPaymentKey` to generate a `SigningKey PaymentKey`. To achieve this, the function above uses the `deterministicSigningKey` function:

```haskell
deterministicSigningKey :: AsType PaymentKey -> Crypto.Seed -> SigningKey PaymentKey
deterministicSigningKey AsPaymentKey seed =
    PaymentSigningKey (Crypto.genKeyDSIGN seed)
```

where the `genKeyDSIGN` function is coming from the `DSIGNAlgorithm` class, of which `Ed25519DSIGN` is an instance, [see here](https://github.com/input-output-hk/cardano-base/blob/master/cardano-crypto-class/src/Cardano/Crypto/DSIGN/Ed25519.hs#L71).

The information that we need the `Ed25519DSIGN` instance of the `DSIGNAlgorithm` class is contained in the `SigningKey PaymentKey` which is defined as:
 
```haskell
newtype SigningKey PaymentKey =
    PaymentSigningKey (Shelley.SignKeyDSIGN StandardCrypto)
```

with the cardano-ledger type `SignKeyDSIGN StandardCrypto` having the following type alias for the cardano-crypto-class:

 ```haskell
type SignKeyDSIGN c = DSIGN.SignKeyDSIGN (DSIGN c)
```

Finally, the distinction between the cryptographic systems on the cardano-ledger side is made in the crypto class, respectively its instance for `StandardCrypto` [here](https://github.com/input-output-hk/cardano-ledger/blob/master/libs/cardano-ledger-core/src/Cardano/Ledger/Crypto.hs#L39)

```haskell
instance Crypto StandardCrypto where
  type DSIGN StandardCrypto = Ed25519DSIGN
  type KES StandardCrypto = Sum6KES Ed25519DSIGN Blake2b_256
  type VRF StandardCrypto = PraosVRF
  type HASH StandardCrypto = Blake2b_256
  type ADDRHASH StandardCrypto = Blake2b_224
```


-----
Then, the mapping to the types used in the `cardano-crypto-class` package where the keys are finally created is defined in the [Keys module](https://github.com/input-output-hk/cardano-ledger/blob/master/libs/cardano-ledger-core/src/Cardano/Ledger/Keys.hs)  
The distinction between the cryptographic systems on the cardano-ledger side is made in the crypto class, respectively its instance for `StandardCrypto` [here](https://github.com/input-output-hk/cardano-ledger/blob/master/libs/cardano-ledger-core/src/Cardano/Ledger/Crypto.hs#L39)
instance of the class `DSIGNAlgorithm` of Ed25519DSIGN is defined [here](https://github.com/input-output-hk/cardano-base/blob/master/cardano-crypto-class/src/Cardano/Crypto/DSIGN/Ed25519.hs#L75)
-----


## Normal and extended Shelley keys

Ten out of 14 keyroles are defined in Cardano.Api.Keys.Shelley (see above). And though they have different names, under the hood they fall in just two categories: normal shelley keys and extended shelley keys.

For the signing keys of these keyroles, we even have a unifying cardano-api type:

```haskell
data ShelleySigningKey =
       -- | A normal ed25519 signing key
       ShelleyNormalSigningKey   (Shelley.SignKeyDSIGN StandardCrypto)
       -- | An extended ed25519 signing key
     | ShelleyExtendedSigningKey Crypto.HD.XPrv
```

A normal Shelley signing key is just what its name implies: A payment key that can be used to sign transactions. Most wallets for Cardano though are HD ([hierarchical deterministic](https://github.com/input-output-hk/technical-docs/blob/main/cardano-components/adrestia/doc/key-concepts/hierarchical-deterministic-wallets.md)) wallets, also called multi-address wallets, and use an extended payment key, also called the root key, from which other (private and public) keys can be derived. HD wallets are similar to those described in [BIP-0032](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#motivation), with the difference that they use (as mentioned above) Ed25519 arithmetic instead of Bitcoins P256K1 arithmethic. the xprv is 128 bytes, but serialised..

The module to work with these extended keys is [Cardano.Crypto.Wallet](https://github.com/input-output-hk/cardano-crypto/blob/develop/src/Cardano/Crypto/Wallet.hs) in the cardano-crypto package. The module contains, for example, the `toXPub` function which the cardano-api uses to convert a `SigningKey PaymentExtendedKey` into a `VerificationKey PaymentExtendedKey` (see `getVerificationKey` in the `PaymentExtendedKey` instance of the `Key` class). The modules also contains the functions to derive child keys from signing and verification keys, these functions are not used by the cardano-api, though, but for example by the cardano-wallet [here](https://github.com/input-output-hk/cardano-wallet/blob/master/lib/wallet/src/Cardano/Wallet/Primitive/AddressDerivation/Shelley.hs#L185) 

The structure is not always the same. Serialisation of extended keys works like the following: [Bip](https://cips.cardano.org/cips/cip16/), the cardano-addresses tool for example encodes keys like this. But the extended keys in the cardano-api also contain the public key, so to convert them, cardano-cli uses `convertBip32SigningKey` which in turn uses `xPrvFromBytes` from the cardano-api:

```haskell
xPrvFromBytes :: ByteString -> Maybe CC.XPrv
xPrvFromBytes bytes
    | BS.length bytes /= 96 = Nothing
    | otherwise = do
        let (prv, cc) = BS.splitAt 64 bytes
        pub <- ed25519ScalarMult (BS.take 32 prv)
        eitherToMaybe $ CC.xprv $ prv <> pub <> cc
  where
    eitherToMaybe :: Either a b -> Maybe b
    eitherToMaybe = either (const Nothing) Just

    ed25519ScalarMult :: ByteString -> Maybe ByteString
    ed25519ScalarMult bs = do
      scalar <- eitherToMaybe . eitherCryptoError $ Ed25519.scalarDecodeLong bs
      pure $ Ed25519.pointEncode $ Ed25519.toPoint scalar
```

This function shows nicely how the pub key is calculated and then added in between the extended private key and the chaincode.

and from cardano-cli:

```haskell
-- | Convert a Ed25519 BIP32 extended signing key (96 bytes) to a @cardano-crypto@
-- style extended signing key.
--
-- Note that both the ITN and @cardano-address@ use this key format.
convertBip32SigningKey
  :: ByteString
  -> Either CardanoAddressSigningKeyConversionError Crypto.XPrv
convertBip32SigningKey signingKeyBs =
  case xPrvFromBytes signingKeyBs of
    Just xPrv -> Right xPrv
    Nothing ->
      Left $ CardanoAddressSigningKeyDeserialisationError signingKeyBs
```

// // If we want to derive child keys like [here](https://input-output-hk.github.io/cardano-wallet/user-guide/cli#key-child), we need a PaymentExtendedKey
// // see how `cardano-wallet` does this [here](https://github.com/input-output-hk/cardano-wallet/blob/master/lib/wallet/src/Cardano/Wallet/Primitive/AddressDerivation/Shelley.hs#L170)
// // it uses `generateNew` and `XPrv` from [cardano-crypto](https://github.com/input-output-hk/cardano-crypto/blob/develop/src/Cardano/Crypto/Wallet.hs)

// // !!! cardano-cli also uses the `cardano-crypto` package for extended key, but instead of `generateNew` it uses `generate` (see below Crypto.HD.generate)

// // For the difference between normal and extended keys see here:
// // chain-wallet from [iog](https://github.com/input-output-hk/chain-wallet-libs/blob/master/doc/CRYPTO.md#master-key-generation-to-cryptographic-key)
// // normal/extended keys on [stack](https://cardano.stackexchange.com/questions/756/difference-between-normal-key-and-extended-key-in-cardano-cli-address-key-ge)
// // good explanation to extended keys [here](https://river.com/learn/terms/x/xprv-extended-private-key/#:~:text=An%20extended%20private%20key%20is,funds%20associated%20with%20that%20key.)

// from the code:
// Shelley payment extended ed25519 keys

// Shelley-era payment keys using extended ed25519 cryptographic keys.
// They can be used for Shelley payment addresses and witnessing
// transactions that spend from these addresses.

// These extended keys are used by HD wallets. So this type provides
// interoperability with HD wallets. The ITN CLI also supported this key type.
// The extended verification keys can be converted (via 'castVerificationKey')
// to ordinary keys (i.e. 'VerificationKey' 'PaymentKey') but this is /not/ the
// case for the signing keys. The signing keys can be used to witness
// transactions directly, with verification via their non-extended verification
// key ('VerificationKey' 'PaymentKey').

// // for verification keys:
// // from Cardano.Ledger.Keys: We wrap the basic `VerKeyDSIGN` in order to add the key role. 
// // `newtype VKey (kd :: KeyRole) c = VKey {unVKey :: DSIGN.VerKeyDSIGN (DSIGN c)}` 

### Interchangeable Shelley keys

Treating all shelley keys uniformely makes sense, for example during the transaction signing process. The `makeShelleySignature` function doesn't need to know what keyrole the signing key has, as the process of signing data works identically for all shelley keys. The only case distinction the function makes is between normal keys and extended keys:

```haskell
makeShelleySignature
  :: Crypto.SignableRepresentation tosign
  => tosign
  -> ShelleySigningKey
  -> Shelley.SignedDSIGN StandardCrypto tosign
makeShelleySignature tosign (ShelleyNormalSigningKey sk) =
    Crypto.signedDSIGN () tosign sk

makeShelleySignature tosign (ShelleyExtendedSigningKey sk) =
    fromXSignature $
      Crypto.HD.sign
        BS.empty  -- passphrase for (unused) in-memory encryption
        sk
        (Crypto.getSignableRepresentation tosign)
  where
    fromXSignature :: Crypto.HD.XSignature
                   -> Shelley.SignedDSIGN StandardCrypto b
    fromXSignature =
        Crypto.SignedDSIGN
      . fromMaybe impossible
      . Crypto.rawDeserialiseSigDSIGN
      . Crypto.HD.unXSignature

    impossible =
      error "makeShelleyKeyWitnessSignature: byron and shelley signature sizes do not match"
```

The cardano-api calls `makeShelleySignature` inside the `makeShelleyKeyWittness` function. To get from the (keyrole sensitive) ShelleyWitnessSigningKey to the ShelleySigningKey, it uses `toShelleySigningKey`.

```haskell
toShelleySigningKey :: ShelleyWitnessSigningKey -> ShelleySigningKey
toShelleySigningKey key = case key of
  -- The cases for normal keys
  WitnessPaymentKey (PaymentSigningKey sk) -> ShelleyNormalSigningKey sk
  ..
  -- The cases for extended keys
  WitnessPaymentExtendedKey (PaymentExtendedSigningKey sk) -> ShelleyExtendedSigningKey sk
  ..
```

As the `makeShelleySignature` function above shows, an extended signing key doesn't have to be converted into a normal signing key to witness a transaction. This is not the case for extended verification keys, though. So, inside the `makeShelleyKeyWitness` function we make the case distinction between normal and extended keys. In case an extended signing key is used, the verification key is derived as follows:

```haskell
getShelleyKeyWitnessVerificationKey (ShelleyExtendedSigningKey sk) =
      (Shelley.coerceKeyRole :: Shelley.VKey Shelley.Payment StandardCrypto
                             -> Shelley.VKey Shelley.Witness StandardCrypto)
    . (\(PaymentVerificationKey vk) -> vk)
    . (castVerificationKey :: VerificationKey PaymentExtendedKey
                           -> VerificationKey PaymentKey)
    . getVerificationKey
    . PaymentExtendedSigningKey
    $ sk
```

The conversion from `VerificationKey PaymentExtendedKey` to `VerificationKey PaymentKey` is done with `castVerificationKey`, a function given by the `CastVerificationKeyRole` class.

`castVerificationKey` can not only be used to convert an extended key into a normal key, but also to convert a normal key into another normal key like for example a genesis key into a payment key:

```haskell
instance CastVerificationKeyRole GenesisKey PaymentKey where
    castVerificationKey (GenesisVerificationKey (Shelley.VKey vk)) =
      PaymentVerificationKey (Shelley.VKey vk)
```

As above example shows again, there is really no difference between the different keyroles under the hood, the names are just used to keep track of what a key is used for.

So, what are the different keyroles used for? A look at the `cardano-cli` helps to distinguish the different categories:

### cardano-cli address key-gen

The address command is used for the payment address commands, and so combined with the `key-gen` command we can generate `PaymentKey`s, `PaymentExtendedKey`s or `ByronKey`s

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

Byron keys are for Byron addresses and witnessing transactions that spend from Byron addresses. As is explained in [Cardano.Api.Keys.Byron](https://github.com/input-output-hk/cardano-node/blob/master/cardano-api/src/Cardano/Api/Keys/Byron.hs), these keys come, because of a design mistake, with a 32byte chaincode used in HD derivation. So, a Byron key is very similar to a Payment extended key, resulting in a very simple `castVerificationKey` function:

```haskell
instance CastVerificationKeyRole ByronKey PaymentExtendedKey where
    castVerificationKey (ByronVerificationKey vk) =
        PaymentExtendedVerificationKey
          (Byron.unVerificationKey vk)
```

### cardano-cli stake-address key-gen

The stake-address command is used for the stake address commands, and so combined with the `key-gen` command we can generate `StakeKey`s

```haskell
runStakeAddressKeyGenToFile
  :: VerificationKeyFile
  -> SigningKeyFile
  -> ExceptT ShelleyStakeAddressCmdError IO ()
runStakeAddressKeyGenToFile (VerificationKeyFile vkFp) (SigningKeyFile skFp) = do
  let skeyDesc = "Stake Signing Key"
  let vkeyDesc = "Stake Verification Key"

  skey <- liftIO $ generateSigningKey AsStakeKey

  let vkey = getVerificationKey skey
```

**StakeExtendedKey?**, cannot be made with cardano-cli

### cardano-cli node key-gen/key-gen-KES/key-gen-VRF (3 keys)

node is for the node operation commands

StakePoolKey, this is a very sensitive key, [see here](https://developers.cardano.org/docs/operate-a-stake-pool/cardano-key-pairs/#cardano-stake-pool-key-pairs)
KesKey [link](https://github.com/input-output-hk/cardano-node/blob/master/doc/stake-pool-operations/7_KES_period.md), KES stands for Key Evolving Signature
VrfKey [link](https://medium.com/dcspark/cardano-algorand-leader-selection-explained-81f71a30b8bb), key take away: every stake pool operator has its own VRF key and this VRF key decides whether or not he is elected for a specific slot

```haskell
runNodeKeyGenCold :: VerificationKeyFile
                  -> SigningKeyFile
                  -> OpCertCounterFile
                  -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenCold (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath)
                  (OpCertCounterFile ocertCtrPath) = do
    skey <- liftIO $ generateSigningKey AsStakePoolKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope ocertCtrPath (Just ocertCtrDesc)
      $ OperationalCertificateIssueCounter initialCounter vkey
  where
    skeyDesc, vkeyDesc, ocertCtrDesc :: TextEnvelopeDescr
    skeyDesc = "Stake Pool Operator Signing Key"
    vkeyDesc = "Stake Pool Operator Verification Key"
    ocertCtrDesc = "Next certificate issue number: "
                <> fromString (show initialCounter)

    initialCounter :: Word64
    initialCounter = 0


runNodeKeyGenKES :: VerificationKeyFile
                 -> SigningKeyFile
                 -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenKES (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsKesKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "KES Signing Key"
    vkeyDesc = "KES Verification Key"

runNodeKeyGenVRF :: VerificationKeyFile -> SigningKeyFile
                 -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenVRF (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsVrfKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelopeWithOwnerPermissions skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "VRF Signing Key"
    vkeyDesc = "VRF Verification Key"
```

### cardano-cli genesis key-gen-genesis/key-gen-delegate/key-gen-utxo

genesis is for the genesis block commands

GenesisUTxOKey, this is one/several key/s to spend the funds created in the genesis file [see here](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/shelley-genesis.md)

Create a Shelley genesis key pair
Create a Shelley genesis delegate key pair
Create a Shelley genesis UTxO key pair


### what im missing

  - GenesisExtendedKey
  - GenesisDelegateExtendedKey
  - ByronKeyLegacy






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
