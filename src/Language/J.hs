{-# LANGUAGE OverloadedStrings #-}

-- | Marshal a limited subset of J arrays into Repa arrays.
module Language.J ( -- * Environment
                    JEnv
                  , jinit
                  , libLinux
                  , bsDispatch
                  , bsOut
                  -- * Repa
                  , JData (..)
                  , getJData
                  -- * FFI (for testing)
                  , JAtom (..)
                  , getAtomInternal
                  ) where

import           Control.Applicative             (pure, (<$>), (<*>))
import qualified Data.Array.Repa                 as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Internal        as BS
import           Data.Functor                    (void)
import           Foreign.C.String                (CString)
import           Foreign.C.Types                 (CChar, CDouble, CInt (..), CLLong (..), CSize (..))
import           Foreign.ForeignPtr              (ForeignPtr, castForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import           Foreign.Marshal                 (alloca, peekArray)
import           Foreign.Ptr                     (FunPtr, Ptr)
import           Foreign.Storable                (peek, sizeOf)
import           System.Posix.ByteString         (RTLDFlags (RTLD_LAZY), RawFilePath, dlopen, dlsym)

-- TODO: windows support
-- (https://hackage.haskell.org/package/Win32-2.10.0.0/docs/System-Win32-DLL.html#v:getProcAddress)

-- TODO: atoms of some sort? more useful https://github.com/jsoftware/stats_jserver4r/blob/4c94fc6df351fab34791aa9d78d158eaefd33b17/source/lib/j2r.c

data J

data JEnv = JEnv (Ptr J) JDoType JGetMType JGetRType

type JDoType = Ptr J -> CString -> IO CInt
type JGetMType = Ptr J -> CString -> Ptr CLLong -> Ptr CLLong -> Ptr (Ptr CLLong) -> Ptr (Ptr ()) -> IO CInt
type JGetRType = Ptr J -> IO CString

foreign import ccall "dynamic" mkJDo :: FunPtr JDoType -> JDoType
foreign import ccall "dynamic" mkJInit :: FunPtr (IO (Ptr J)) -> IO (Ptr J)
foreign import ccall "dynamic" mkJGetM :: FunPtr JGetMType -> JGetMType
foreign import ccall "dynamic" mkJGetR :: FunPtr JGetRType -> JGetRType


foreign import ccall unsafe "memcpy" memcpy :: Ptr a -> Ptr b -> CSize -> IO (Ptr ())

-- | Expected 'RawFilePath' to the library on a Linux machine.
libLinux :: RawFilePath
libLinux = "/usr/lib/x86_64-linux-gnu/libj.so"

-- | Get a J environment
--
-- Don't pass the resultant 'JEnv' between threads
jinit :: RawFilePath -- ^ Path to J library
      -> IO JEnv
jinit libFp = do
    libj <- dlopen libFp [RTLD_LAZY]
    jt <- mkJInit =<< dlsym libj "JInit"
    let jeval = mkJDo <$> dlsym libj "JDo"
    let jread = mkJGetM <$> dlsym libj "JGetM"
    let jOut = mkJGetR <$> dlsym libj "JGetR"
    JEnv jt <$> jeval <*> jread <*> jOut

-- | Send some J code to the environment.
bsDispatch :: JEnv -> BS.ByteString -> IO ()
bsDispatch (JEnv ctx jdo _ _) bs =
    void $ BS.useAsCString bs $ jdo ctx

-- | Read last output
--
-- For debugging
bsOut :: JEnv -> IO BS.ByteString
bsOut (JEnv ctx _ _ jout) = BS.packCString =<< jout ctx

getJData :: R.Shape sh
         => JEnv -> BS.ByteString -- ^ Name of the value in question
         -> IO (JData sh)
getJData jenv bs = jData <$> getAtomInternal jenv bs

getAtomInternal :: JEnv -> BS.ByteString -- ^ Name of the value in question
                -> IO JAtom
getAtomInternal (JEnv ctx _ jget _) bs = do
    BS.useAsCString bs $ \name ->
        alloca $ \t ->
        alloca $ \s ->
        alloca $ \r ->
        alloca $ \d -> do
            jget ctx name t r s d
            ty' <- intToJType <$> peek t
            rank' <- peek r
            let intRank = fromIntegral rank'
            shape' <- peekArray intRank =<< peek s
            let mult = case ty' of
                    JBool    -> sizeOf (undefined :: CChar)
                    JChar    -> sizeOf (undefined :: CChar)
                    JInteger -> sizeOf (undefined :: CInt)
                    JDouble  -> sizeOf (undefined :: CDouble)
            let resBytes = mult * intRank
            res <- mallocForeignPtrBytes resBytes
            let arrSz = fromIntegral mult * fromIntegral (product shape')
            withForeignPtr res $ \r' -> do
                d' <- peek d
                memcpy r' d' arrSz
            pure $ JAtom ty' shape' res

data JAtom = JAtom { ty     :: !JType
                   , shape  :: ![CLLong]
                   , datums :: !(ForeignPtr ()) -- ^ \'data\' is a Haskell keyword
                   }

data JData sh = JIntArr !(R.Array RF.F sh CInt)
              | JDoubleArr !(R.Array RF.F sh CDouble)
              | JBoolArr !(R.Array RF.F sh CChar)
              | JString !BS.ByteString

-- | J types
data JType = JBool
           | JChar
           | JInteger
           | JDouble

intToJType :: CLLong -> JType
intToJType 1 = JBool
intToJType 2 = JChar
intToJType 4 = JInteger
intToJType 8 = JDouble
intToJType _ = error "Unsupported type!"

jData :: R.Shape sh => JAtom -> JData sh
jData (JAtom JInteger sh fp) = JIntArr $ RF.fromForeignPtr (R.shapeOfList $ fmap fromIntegral sh) (castForeignPtr fp)
jData (JAtom JDouble sh fp)  = JDoubleArr $ RF.fromForeignPtr (R.shapeOfList $ fmap fromIntegral sh) (castForeignPtr fp)
jData (JAtom JBool sh fp)    = JBoolArr $ RF.fromForeignPtr (R.shapeOfList $ fmap fromIntegral sh) (castForeignPtr fp)
jData (JAtom JChar [l] fp)   = JString $ BS.fromForeignPtr (castForeignPtr fp) 0 (fromIntegral l)
jData (JAtom JChar _ _)      = error "Not supported."
