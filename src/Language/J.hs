{-# LANGUAGE OverloadedStrings #-}

module Language.J ( -- * Environment
                    JEnv (..)
                  , jinit
                  , libLinux
                  , bsDispatch
                  , bsOut
                  -- * FFI
                  , JDoType
                  , JGetMType
                  , JGetRType
                  , J
                  , mkJDo
                  , mkJInit
                  , mkJGetM
                  -- * Marshaling (to Haskell)
                  , JType (..)
                  , intToJType
                  , JAtom (..)
                  , getAtomInternal
                  -- * Repa
                  , JData (..)
                  , jData
                  ) where

import           Control.Applicative             (pure, (<$>), (<*>))
import           Data.ByteString                 as BS
import           Data.Functor                    (void)
import           Foreign.C.String                (CString)
import           Foreign.C.Types                 (CChar, CDouble, CInt (..),
                                                  CLLong (..), CSize (..))
import           Foreign.ForeignPtr              (ForeignPtr, castForeignPtr,
                                                  mallocForeignPtrBytes,
                                                  withForeignPtr)
import           Foreign.Marshal                 (alloca, peekArray)
import           Foreign.Ptr                     (FunPtr, Ptr)
import           Foreign.Storable                (peek, sizeOf)
import           System.Posix.ByteString         (RTLDFlags (RTLD_LAZY),
                                                  RawFilePath, dlopen, dlsym)
import qualified Data.Array.Repa                 as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RF

-- TODO: windows support
-- (https://hackage.haskell.org/package/Win32-2.10.0.0/docs/System-Win32-DLL.html#v:getProcAddress)

-- TODO: atoms of some sort? more useful https://github.com/jsoftware/stats_jserver4r/blob/4c94fc6df351fab34791aa9d78d158eaefd33b17/source/lib/j2r.c

data J

data JEnv = JEnv { context   :: Ptr J
                 , evaluator :: JDoType
                 , reader    :: JGetMType
                 , output    :: JGetRType
                 }

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
bsOut :: JEnv -> IO BS.ByteString
bsOut (JEnv ctx _ _ jout) = BS.packCString =<< jout ctx

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
            withForeignPtr res $ \r -> do
                d' <- peek d
                memcpy r d' arrSz
            pure $ JAtom ty' rank' shape' res

data JAtom = JAtom { ty     :: !JType
                   , rank   :: !CLLong
                   , shape  :: ![CLLong]
                   , datums :: !(ForeignPtr ()) -- ^ \'data\' is a Haskell keyword
                   }

data JData sh = JIntArr !(R.Array RF.F sh CInt)
              | JDoubleArr !(R.Array RF.F sh CDouble)
              | JString !BS.ByteString

-- | J types
data JType = JBool
           | JChar
           | JInteger
           | JDouble
           deriving (Show, Eq)

intToJType :: CLLong -> JType
intToJType 1 = JBool
intToJType 2 = JChar
intToJType 4 = JInteger
intToJType 8 = JDouble
intToJType _ = error "Unsupported type!"

jData :: R.Shape sh => JAtom -> JData sh
jData (JAtom JInteger _ sh fp) = JIntArr $ RF.fromForeignPtr (R.shapeOfList $ fmap fromIntegral sh) (castForeignPtr fp)
jData (JAtom JDouble _ sh fp) = JDoubleArr $ RF.fromForeignPtr (R.shapeOfList $ fmap fromIntegral sh) (castForeignPtr fp)

