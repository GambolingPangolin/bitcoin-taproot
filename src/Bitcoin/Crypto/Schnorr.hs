module Bitcoin.Crypto.Schnorr (
    Bip340Sig,
    sign,
    verify,
) where

import Bitcoin.Crypto.Schnorr.Internal (
    context,
    keyPairCreate,
    schnorrSign,
    schnorrVerify,
    xOnlyPubKeyFromPubKey,
 )
import Crypto.Hash (SHA256 (SHA256), hashWith)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (
    unsafePackMallocCStringLen,
    unsafeUseAsCString,
    unsafeUseAsCStringLen,
 )
import Foreign (allocaBytes, free, mallocBytes, nullPtr)
import Haskoin (Msg, PubKey, SecKey, exportPubKey, getMsg, getSecKey)
import System.IO.Unsafe (unsafePerformIO)

-- | TODO serialization
newtype Bip340Sig = Bip340Sig {unBip340Sig :: ByteString}
    deriving (Eq, Show)

-- | Sign a message according to BIP 340
sign ::
    -- | Secret key
    SecKey ->
    -- | Message
    Msg ->
    -- | Randomness
    ByteString ->
    -- | Signature
    Maybe Bip340Sig
sign secKey message anyRand = unsafePerformIO $
    unsafeUseAsCString (getSecKey secKey) $ \secKeyPtr ->
        allocaBytes 96 $ \keyPairPtr -> do
            -- The 'SecKey' API guarantees that the following call will succeed
            _ <- keyPairCreate context keyPairPtr secKeyPtr
            unsafeUseAsCString (getMsg message) $ \messageCStr -> do
                unsafeUseAsCString rand32 $ \rand32CStr -> do
                    sigPtr <- mallocBytes 64
                    returnVal <- schnorrSign context sigPtr messageCStr keyPairPtr rand32CStr
                    if returnVal == 1
                        then Just . Bip340Sig <$> unsafePackMallocCStringLen (sigPtr, 64)
                        else Nothing <$ free sigPtr
  where
    rand32 = BA.convert $ hashWith SHA256 anyRand

-- | Verify a message according to BIP 340
verify ::
    PubKey ->
    -- | Message
    Msg ->
    -- | Signature
    Bip340Sig ->
    Bool
verify pubKey message sig = unsafePerformIO $
    unsafeUseAsCString (exportPubKey False pubKey) $ \pubKeyPtr ->
        allocaBytes 32 $ \xOnlyPubKeyPtr -> do
            _ <- xOnlyPubKeyFromPubKey context xOnlyPubKeyPtr nullPtr pubKeyPtr
            unsafeUseAsCStringLen (getMsg message) $ \(messageCStr, messageSize) ->
                unsafeUseAsCString (unBip340Sig sig) $ \signatureCStr ->
                    (== 1)
                        <$> schnorrVerify
                            context
                            signatureCStr
                            messageCStr
                            (fromIntegral messageSize)
                            xOnlyPubKeyPtr
