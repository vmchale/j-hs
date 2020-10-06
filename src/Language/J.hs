{-# LANGUAGE FlexibleContexts  #-}
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
                  , setJData
                  ) where

import           Control.Applicative             (pure, (<$>), (<*>))
import qualified Data.Array.Repa                 as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Internal        as BS
import           Data.Functor                    (void)
import           Foreign.C.String                (CString)
import           Foreign.C.Types                 (CChar, CDouble, CInt (..), CLLong (..))
import           Foreign.ForeignPtr              (ForeignPtr, castForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import           Foreign.Marshal                 (alloca, copyArray, mallocBytes, peekArray, pokeArray)
import           Foreign.Ptr                     (FunPtr, Ptr, plusPtr)
import           Foreign.Storable                (Storable, peek, pokeByteOff, sizeOf)
import           System.Posix.ByteString         (RTLDFlags (RTLD_LAZY), RawFilePath, dlopen, dlsym)

-- TODO: windows support
-- (https://hackage.haskell.org/package/Win32-2.10.0.0/docs/System-Win32-DLL.html#v:getProcAddress)

-- https://github.com/jsoftware/stats_jserver4r/blob/4c94fc6df351fab34791aa9d78d158eaefd33b17/source/lib/j2r.c
--
-- see: https://github.com/jsoftware/stats_jserver4r/blob/4c94fc6df351fab34791aa9d78d158eaefd33b17/source/lib/r2j.c
-- https://github.com/jsoftware/stats_jserver4r/blob/4c94fc6df351fab34791aa9d78d158eaefd33b17/source/lib/base.c#L116

data J

data JEnv = JEnv (Ptr J) JDoType JGetMType JGetRType JSetAType

type JDoType = Ptr J -> CString -> IO CInt
type JGetMType = Ptr J -> CString -> Ptr CLLong -> Ptr CLLong -> Ptr (Ptr CLLong) -> Ptr (Ptr CChar) -> IO CInt
type JGetRType = Ptr J -> IO CString
type JSetAType = Ptr J -> CLLong -> CString -> CLLong -> Ptr () -> IO CInt

foreign import ccall "dynamic" mkJDo :: FunPtr JDoType -> JDoType
foreign import ccall "dynamic" mkJInit :: FunPtr (IO (Ptr J)) -> IO (Ptr J)
foreign import ccall "dynamic" mkJGetM :: FunPtr JGetMType -> JGetMType
foreign import ccall "dynamic" mkJGetR :: FunPtr JGetRType -> JGetRType
foreign import ccall "dynamic" mkJSetA :: FunPtr JSetAType -> JSetAType

-- | Expected 'RawFilePath' to the library on a Linux machine.
libLinux :: RawFilePath
libLinux = "/usr/lib/x86_64-linux-gnu/libj.so"

-- | Get a J environment
--
-- Passing the resultant 'JEnv' between threads can cause unexpected bugs.
jinit :: RawFilePath -- ^ Path to J library
      -> IO JEnv
jinit libFp = do
    libj <- dlopen libFp [RTLD_LAZY]
    jt <- mkJInit =<< dlsym libj "JInit"
    let jeval = mkJDo <$> dlsym libj "JDo"
    let jread = mkJGetM <$> dlsym libj "JGetM"
    let jOut = mkJGetR <$> dlsym libj "JGetR"
    let jSet = mkJSetA <$> dlsym libj "JSetA"
    JEnv jt <$> jeval <*> jread <*> jOut <*> jSet

-- | Send some J code to the environment.
bsDispatch :: JEnv -> BS.ByteString -> IO ()
bsDispatch (JEnv ctx jdo _ _ _) bs =
    void $ BS.useAsCString bs $ jdo ctx

-- | Read last output
--
-- For debugging
bsOut :: JEnv -> IO BS.ByteString
bsOut (JEnv ctx _ _ jout _) = BS.packCString =<< jout ctx

getJData :: R.Shape sh
         => JEnv -> BS.ByteString -- ^ Name of the value in question
         -> IO (JData sh)
getJData jenv bs = jData <$> getAtomInternal jenv bs

getAtomInternal :: JEnv -> BS.ByteString -- ^ Name of the value in question
                -> IO JAtom
getAtomInternal (JEnv ctx _ jget _ _) bs = do
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
            let arrSz = mult * fromIntegral (product shape')
            withForeignPtr res $ \r' -> do
                d' <- peek d
                copyArray r' d' arrSz
            pure $ JAtom ty' shape' res

data JAtom = JAtom !JType ![CLLong] !(ForeignPtr CChar)

-- | J data backed by repa array
data JData sh = JIntArr !(R.Array RF.F sh CInt)
              | JDoubleArr !(R.Array RF.F sh CDouble)
              | JBoolArr !(R.Array RF.F sh CChar)
              | JString !BS.ByteString

setJData :: (R.Shape sh) => JEnv -> BS.ByteString -- ^ Name
                         -> JData sh -> IO CInt
setJData (JEnv ctx _ _ _ jset) name (JIntArr iarr) = BS.useAsCStringLen name $ \(n, sz) -> do
    (ds, d) <- repaArr JInteger iarr
    jset ctx (fromIntegral sz) n ds d
setJData (JEnv ctx _ _ _ jset) name (JDoubleArr iarr) = BS.useAsCStringLen name $ \(n, sz) -> do
    (ds, d) <- repaArr JDouble iarr
    jset ctx (fromIntegral sz) n ds d
setJData _ _ _ = error "Yet to be implemented"

-- | Return a @'Ptr' ()@ suitable to be passed to @JSetA@
--
-- To be used on integer and double arrays
repaArr :: (R.Shape sh, Storable e) => JType -> R.Array RF.F sh e -> IO (CLLong, Ptr ())
repaArr jty arr = do
    let (rank', sh) = repaSize arr
        sz = product sh
    let wid = 32 + 8 * (rank' + sz)
    ptr <- mallocBytes (fromIntegral wid)
    pokeByteOff ptr 0 (227 :: CLLong)
    pokeByteOff ptr (sizeOf (undefined :: CLLong)) (jTypeToInt jty)
    pokeByteOff ptr (2 * sizeOf (undefined :: CLLong)) sz
    pokeByteOff ptr (3 * sizeOf (undefined :: CLLong)) rank'
    let dimOff = 4 * sizeOf (undefined :: CLLong)
    pokeArray (ptr `plusPtr` dimOff) sh
    let dataOff = dimOff + fromIntegral rank' * sizeOf (undefined :: CLLong)
    withForeignPtr (RF.toForeignPtr arr) $ \src ->
        copyArray (ptr `plusPtr` dataOff) src (fromIntegral sz)
    pure (wid, ptr)

repaSize :: (R.Source r e, R.Shape sh) => R.Array r sh e -> (CLLong, [CLLong])
repaSize arr = let sh = R.extent arr in (fromIntegral $ R.rank sh, fromIntegral <$> R.listOfShape sh)

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

jTypeToInt :: JType -> CLLong
jTypeToInt JBool    = 1
jTypeToInt JChar    = 2
jTypeToInt JInteger = 4
jTypeToInt JDouble  = 8

jData :: R.Shape sh => JAtom -> JData sh
jData (JAtom JInteger sh fp) = JIntArr $ RF.fromForeignPtr (R.shapeOfList $ fmap fromIntegral sh) (castForeignPtr fp)
jData (JAtom JDouble sh fp)  = JDoubleArr $ RF.fromForeignPtr (R.shapeOfList $ fmap fromIntegral sh) (castForeignPtr fp)
jData (JAtom JBool sh fp)    = JBoolArr $ RF.fromForeignPtr (R.shapeOfList $ fmap fromIntegral sh) (castForeignPtr fp)
jData (JAtom JChar [l] fp)   = JString $ BS.fromForeignPtr (castForeignPtr fp) 0 (fromIntegral l)
jData (JAtom JChar _ _)      = error "Not supported."
