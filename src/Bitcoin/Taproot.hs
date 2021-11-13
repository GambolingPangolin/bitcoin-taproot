{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Bitcoin.Taproot (
    ExtFlag,
    signInput,
    signatureDigest,
) where

import Bitcoin.Crypto.Schnorr (Bip340Sig, sign)
import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Crypto.Hash (SHA256 (SHA256), hashWith)
import Data.Binary (Binary (put), Put)
import Data.Binary.Put (runPut)
import Data.Bool (bool)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe, isJust)
import Data.Word (Word32, Word8)
import Haskoin (
    Input,
    SecKey,
    SigHash (SigHash),
    Tx,
    TxIn,
    TxOut,
    VarString (VarString),
 )
import qualified Haskoin as H

type ExtFlag = Word8

signInput ::
    ExtFlag ->
    [Input] ->
    Tx ->
    Int ->
    Maybe ByteString ->
    -- | Secret key
    SecKey ->
    -- | Randomness
    ByteString ->
    Maybe (Bip340Sig, SigHash)
signInput extFlag inputs tx inputIndex annex secKey rand32 =
    (,) <$> sign secKey message rand32 <*> pure sigHash
  where
    Just message =
        H.msg
            . BA.convert @_ @ByteString
            . hashWith SHA256
            . BSL.toStrict
            $ signatureDigest extFlag inputs tx inputIndex annex
    sigHash = fromMaybe H.sigHashAll . H.sigHashType $ inputs !! inputIndex

-- | Calculate the signature digest for a taproot output
signatureDigest ::
    ExtFlag ->
    [Input] ->
    Tx ->
    -- | Input index for which we sign
    Int ->
    -- | Taproot annex
    Maybe ByteString ->
    BSL.ByteString
signatureDigest extFlag inputs tx inputIndex annexM = runPut $ do
    put sigHashValue
    put $ H.txVersion tx
    put $ H.txLockTime tx
    unless (H.hasAnyoneCanPayFlag sigHash) $ do
        putSpent $ \_ txIn -> H.prevOutput txIn
        putSpent $ \txOut _ -> H.outValue txOut
        putSpent $ \txOut _ -> H.scriptOutput txOut
        putSpent $ \_ txIn -> H.txInSequence txIn
    unless (H.isSigHashNone sigHash || H.isSigHashSingle sigHash) $
        mapM_ put (H.txOut tx)
    put spendType
    if H.hasAnyoneCanPayFlag sigHash
        then do
            put spentOutPoint
            put $ H.outValue spentTxOut
            put $ H.scriptOutput spentTxOut
            put $ H.txInSequence thisTxIn
        else put $ fromIntegral @_ @Word32 inputIndex
    mapM_ (putSha256 . put . VarString) annexM
    when (H.isSigHashSingle sigHash) . put $ H.txOut tx !! fromIntegral inputIndex
  where
    spendType = extFlag * 2 + bool 0 1 hasAnnex
    hasAnnex = isJust annexM
    sigHash@(SigHash sigHashValue) = fromMaybe H.sigHashAll $ H.sigHashType thisInput

    thisInput = inputs !! inputIndex
    thisTxIn = H.txIn tx !! inputIndex

    spentOutPoint = H.prevOutput thisTxIn
    spentTxOut = fromMaybe missingSpentTxOut $ prevTxOut thisInput spentOutPoint
    missingSpentTxOut = error "signatureDigest: spent TxOut missing"

    putSpent :: Binary a => (TxOut -> TxIn -> a) -> Put
    putSpent f = mapM_ put $ zipWith (withTxOut f) inputs (H.txIn tx)
    withTxOut f theInput txIn =
        f (fromMaybe missingPrevTxOut $ prevTxOut theInput (H.prevOutput txIn)) txIn
    missingPrevTxOut = error "missing a previous TxOut"

    prevTxOut theInput outPoint =
        ((!! vout outPoint) . H.txOut <$> H.nonWitnessUtxo theInput)
            <|> H.witnessUtxo theInput
    vout = fromIntegral . H.outPointIndex

putSha256 :: Put -> Put
putSha256 =
    put @ByteString
        . BA.convert
        . hashWith SHA256
        . BSL.toStrict
        . runPut
