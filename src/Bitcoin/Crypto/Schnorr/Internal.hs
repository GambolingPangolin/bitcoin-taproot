module Bitcoin.Crypto.Schnorr.Internal (
    Context,
    context,
    schnorrSign,
    schnorrVerify,
    keyPairCreate,
    xOnlyPubKeyFromPubKey,
) where

import Foreign (Ptr)
import Foreign.C (CInt (..), CSize (..), CString, CUInt (..))
import System.IO.Unsafe (unsafePerformIO)

data Context

foreign import ccall safe "secp256k1.h secp256k1_context_create"
    contextCreate :: CUInt -> IO (Ptr Context)

context :: Ptr Context
context = unsafePerformIO $ contextCreate 0x0301 -- sign & verify
{-# NOINLINE context #-}

foreign import ccall unsafe "secp256k1.h secp256k1_schnorrsig_sign"
    schnorrSign ::
        Ptr Context ->
        -- | Signature
        CString ->
        -- | Message
        CString ->
        -- | Keypair
        CString ->
        -- | Randomness
        CString ->
        IO CInt

foreign import ccall unsafe "secp256k1.h secp256k1_schnorrsig_verify"
    schnorrVerify ::
        Ptr Context ->
        -- | Signature
        CString ->
        -- | Message
        CString ->
        -- | Message length
        CSize ->
        -- | X-Only pubkey
        CString ->
        IO CInt

foreign import ccall unsafe "secp256k1.h secp256k1_keypair_create"
    keyPairCreate ::
        Ptr Context ->
        -- | Keypair
        CString ->
        -- | Secret key
        CString ->
        IO CInt

foreign import ccall unsafe "secp256k1.h secp256k1_xonly_pubkey_from_pubkey"
    xOnlyPubKeyFromPubKey ::
        Ptr Context ->
        -- | X-only pubkey
        CString ->
        -- | Parity
        Ptr CInt ->
        -- | Pubkey
        CString ->
        IO CInt
