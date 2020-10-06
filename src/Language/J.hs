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
                  ) where

import           Control.Applicative     (pure, (<$>), (<*>))
import           Data.ByteString         as BS
import           Data.Functor            (void)
import           Foreign.C.String        (CString)
import           Foreign.C.Types         (CInt (..), CLLong (..), CSize (..))
import           Foreign.ForeignPtr      (ForeignPtr, mallocForeignPtrBytes,
                                          withForeignPtr)
import           Foreign.Marshal         (alloca, peekArray)
import           Foreign.Ptr             (FunPtr, Ptr)
import           Foreign.Storable        (peek)
import           System.Posix.ByteString (RTLDFlags (RTLD_LAZY), RawFilePath,
                                          dlopen, dlsym)
-- TODO: windows support
-- (https://hackage.haskell.org/package/Win32-2.10.0.0/docs/System-Win32-DLL.html#v:getProcAddress)

-- TODO: atoms of some sort? more useful https://github.com/jsoftware/stats_jserver4r/blob/4c94fc6df351fab34791aa9d78d158eaefd33b17/source/lib/j2r.c
-- https://github.com/jsoftware/stats_jserver4r/blob/4c94fc6df351fab34791aa9d78d158eaefd33b17/source/lib/base.c#L39
-- https://github.com/jsoftware/stats_jserver4r/blob/4c94fc6df351fab34791aa9d78d158eaefd33b17/source/lib/base.h

data J

data JEnv = JEnv { context   :: Ptr J
                 , evaluator :: JDoType
                 , reader    :: JGetMType
                 , output    :: JGetRType
                 }

type JDoType = Ptr J -> CString -> IO CInt
type JGetMType = Ptr J -> CString -> Ptr CLLong -> Ptr CLLong -> Ptr (Ptr CLLong) -> Ptr (Ptr CLLong) -> IO CInt
type JGetRType = Ptr J -> IO CString

foreign import ccall "dynamic" mkJDo :: FunPtr JDoType -> JDoType
foreign import ccall "dynamic" mkJInit :: FunPtr (IO (Ptr J)) -> IO (Ptr J)
foreign import ccall "dynamic" mkJGetM :: FunPtr JGetMType -> JGetMType
foreign import ccall "dynamic" mkJGetR :: FunPtr JGetRType -> JGetRType


foreign import ccall unsafe "memcpy" memcpy :: Ptr a -> Ptr a -> CSize -> IO (Ptr ())

libLinux :: RawFilePath
libLinux = "/usr/lib/x86_64-linux-gnu/libj.so"

-- repa? massiv...

-- | Get a J environment
--
-- Don't pass the resultant 'JEnv' between threads; that fails for whatever
-- reason
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

getAtomInternal :: JEnv -> BS.ByteString -> IO JAtom
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
            res <- mallocForeignPtrBytes intRank
            let arrSz = fromIntegral $ product shape'
            withForeignPtr res $ \r -> do
                d' <- peek d
                memcpy r d' arrSz
            pure $ JAtom ty' rank' shape' res

data JAtom = JAtom { ty     :: !JType
                   , rank   :: !CLLong
                   , shape  :: ![CLLong]
                   , datums :: !(ForeignPtr CLLong) -- ^ \'data\' is a Haskell keyword
                   }

-- | J types
data JType = JBool
           | JChar
           | JInteger
           | JDouble
           | JComplex
           | JBoxed
           deriving (Show, Eq)

intToJType :: CLLong -> JType
intToJType 1  = JBool
intToJType 2  = JChar
intToJType 4  = JInteger
intToJType 8  = JDouble
intToJType 16 = JComplex
intToJType 32 = JBoxed
intToJType _  = error "Unknown type!"
