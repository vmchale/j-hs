{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Marshal a limited subset of J arrays into Repa arrays.
--
-- = Tutorial
--
-- Suppose we wish to perform linear regression. In J we could do:
--
-- @
-- xs := 1 2 3
-- ys := 2 4 6
--
-- reg_result =: ys %. xs ^/ i.2
-- @
--
-- To do this with Haskell data:
--
-- @
-- do
--    jenv <- 'jinit' 'libLinux'
--
--    let hsArr0 = R.fromListUnboxed (R.ix1 3) [1.0,2.0,3.0]
--        hsArr1 = R.fromListUnboxed (R.ix1 3) [2.0,4.0,6.0]
--        jArr0 = 'JDoubleArr' $ R.copyS $ R.map (realToFrac :: Double -> CDouble) hsArr0
--        jArr1 = 'JDoubleArr' $ R.copyS $ R.map (realToFrac :: Double -> CDouble) hsArr1
--
--    'setJData' jenv "xs" jArr0
--    'setJData' jenv "ys" jArr1
--
--    'bsDispatch' jenv "reg_result =: ys %. xs ^/ i.2"
--
--    'JDoubleArr' res <- 'getJData' jenv "reg_result"
--    R.toList res
-- @
--
-- There are three steps to do the calculation, plus one to get a J environment.
--
--     (1) Use 'jinit' with the appropriate file path for your platform
--
--     2. Marshal Haskell values and send them to the J environment. To do so, we
--     use 'setJData', which takes a 'JData' containing a repa array or
--     a string.
--
--     3. Perform calculations within the J environment. Here, we use
--     'bsDispatch' to compute some results and assign them within J
--
--     4. Marshal J values back to Haskell. We use 'getJData'.
--
--
--  Since marshaling data between J and Haskell is expensive, it's best to do as
--  much computation as possible in J.
--
--  == Loading Profile
--
--  If you would like to use user libraries, you need to use 'jLoad' on the
--  'JEnv'. As an example:
--
--  @
--  do
--      jenv <- 'jinit' 'libLinux'
--      'jLoad' jenv ('linuxProfile' "9.01")
--      'bsDispatch' 'jenv' "load'tables/csv'"
--  @
--
--  This will load the CSV addon, assuming it is installed.
--
--  = FFI
--
--  If you want to marshal data yourself, say to use a @Vector@, look at 'JEnv'.
module Language.J ( -- * Environment
                    JEnv (..)
                  , jinit
                  , jLoad
                  , Profile (..)
                  , linuxProfile
                  , macProfile
                  , windowsProfile
#ifndef mingw32_HOST_OS
                  , libLinux
                  , libMac
                  , profLinux
#else
                  , libWindows
#endif
                  , bsDispatch
                  , bsOut
                  , JVersion
                  -- * Repa
                  , JData (..)
                  , getJData
                  , setJData
                  -- * FFI
                  , J
                  , JDoType
                  , JGetMType
                  , JGetRType
                  , JSetAType
                  ) where

import           Control.Applicative             (pure, (<$>), (<*>))
import qualified Data.Array.Repa                 as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Char8           as ASCII
import qualified Data.ByteString.Internal        as BS
import           Data.Complex                    (Complex (..))
import           Data.Functor                    (void)
import           Data.Semigroup                  ((<>))
import           Foreign.C.String                (CString)
import           Foreign.C.Types                 (CChar, CDouble, CInt (..), CLLong (..))
import           Foreign.ForeignPtr              (ForeignPtr, castForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import           Foreign.Marshal                 (alloca, copyArray, mallocBytes, peekArray, pokeArray)
import           Foreign.Ptr                     (FunPtr, Ptr, castPtrToFunPtr, plusPtr)
import           Foreign.Storable                (Storable, peek, pokeByteOff, sizeOf)
import           System.Info                     (arch)
#ifndef mingw32_HOST_OS
import           System.Posix.ByteString         (RTLDFlags (RTLD_LAZY), RawFilePath, dlopen, dlsym)
#else
import           System.Win32.DLL                (getProcAddress, loadLibrary)
#endif

-- Upstream reference
-- https://github.com/jsoftware/stats_jserver4r/blob/4c94fc6df351fab34791aa9d78d158eaefd33b17/source/lib/j2r.c
-- https://github.com/jsoftware/stats_jserver4r/blob/4c94fc6df351fab34791aa9d78d158eaefd33b17/source/lib/r2j.c

-- | Abstract context
data J

data JEnv = JEnv { context   :: Ptr J
                 , evaluator :: JDoType
                 , reader    :: JGetMType
                 , out       :: JGetRType
                 , setter    :: JSetAType
                 }

type JDoType = Ptr J -> CString -> IO CInt
type JGetMType = Ptr J -> CString -> Ptr CLLong -> Ptr CLLong -> Ptr (Ptr CLLong) -> Ptr (Ptr CChar) -> IO CInt
type JGetRType = Ptr J -> IO CString
type JSetAType = Ptr J -> CLLong -> CString -> CLLong -> Ptr () -> IO CInt

foreign import ccall "dynamic" mkJDo :: FunPtr JDoType -> JDoType
foreign import ccall "dynamic" mkJInit :: FunPtr (IO (Ptr J)) -> IO (Ptr J)
foreign import ccall "dynamic" mkJGetM :: FunPtr JGetMType -> JGetMType
foreign import ccall "dynamic" mkJGetR :: FunPtr JGetRType -> JGetRType
foreign import ccall "dynamic" mkJSetA :: FunPtr JSetAType -> JSetAType

type JVersion = [Int]

squashVersion :: JVersion -> String
squashVersion = concatMap show

squashVersionBS :: JVersion -> BS.ByteString
squashVersionBS = ASCII.pack . squashVersion

#ifndef mingw32_HOST_OS
-- | Expected 'RawFilePath' to the library on a Linux machine.
libLinux :: RawFilePath
libLinux = "/usr/lib/" <> ASCII.pack arch <> "-linux-gnu/libj.so"

-- | Expected 'RawFilePath' to the library on Mac.
libMac :: JVersion -> RawFilePath
libMac v = "/Applications/j64-" <> squashVersionBS v <> "/bin/libj.dylib"
#else
-- | @since 0.1.1.0
libWindows :: JVersion -> FilePath
libWindows v = "C:\\Program Files\\J" <> squashVersion v <> "\\bin\\j.dll"
#endif

profLinux :: BS.ByteString -> BS.ByteString
profLinux v = "/etc/j/" <> v <> "/profile.ijs"

binpathLinux :: BS.ByteString
binpathLinux = "/usr/bin"

dllLinux :: BS.ByteString -> BS.ByteString
dllLinux v = "libj.so." <> v

-- | @since 0.1.2.0
linuxProfile :: BS.ByteString -- ^ J version, e.g. @"9.01"@
             -> Profile
linuxProfile ver = Profile (profLinux ver) binpathLinux (dllLinux ver)

-- | @since 0.1.2.0
macProfile :: JVersion
           -> Profile
macProfile v =
    let binPathMac = "/Applications/j64-" <> squashVersionBS v <> "/bin"
        in Profile (binPathMac <> "/profile.ijs") binPathMac (binPathMac <> "/libj.dylib")

-- | @since 0.1.2.0
windowsProfile :: JVersion
               -> Profile
windowsProfile v =
    let binPathWindows = "C:\\Program Files\\J" <> squashVersionBS v <> "\\bin"
        in Profile (binPathWindows <> "\\profile.ijs") binPathWindows (binPathWindows <> "j.dll")

-- | @since 0.1.2.0
data Profile = Profile { profPath :: BS.ByteString -- ^ @profile.ijs@
                       , binPath  :: BS.ByteString
                       , dllName  :: BS.ByteString
                       }

-- | Load user profile.
--
-- @since 0.1.2.0@
jLoad :: JEnv
      -> Profile
      -> IO ()
jLoad jenv (Profile fp bin dll) = bsDispatch jenv ("(3 : '0!:0 y')<'"<> fp <> "'[BINPATH_z_=:'" <> bin <> "'[LIBFILE_z_=:'" <> dll <> "'[ARGV_z_=:''")

-- | Get a J environment
--
-- Passing the resultant 'JEnv' between threads can cause unexpected bugs.
#ifndef mingw32_HOST_OS
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
#else
jinit :: FilePath
      -> IO JEnv
jinit libFp = do
    libj <- loadLibrary libFp
    jt <- mkJInit . castPtrToFunPtr =<< getProcAddress libj "JInit"
    let jeval = mkJDo . castPtrToFunPtr <$> getProcAddress libj "JDo"
    let jread = mkJGetM . castPtrToFunPtr <$> getProcAddress libj "JGetM"
    let jOut = mkJGetR . castPtrToFunPtr <$> getProcAddress libj "JGetR"
    let jSet = mkJSetA . castPtrToFunPtr <$> getProcAddress libj "JSetA"
    JEnv jt <$> jeval <*> jread <*> jOut <*> jSet
#endif


-- | Send some J code to the environment.
bsDispatch :: JEnv -> BS.ByteString -> IO ()
bsDispatch (JEnv ctx jdo _ _ _) bs =
    void $ BS.useAsCString bs $ jdo ctx

-- | Read last output
--
-- For debugging
bsOut :: JEnv -> IO BS.ByteString
bsOut (JEnv ctx _ _ jout _) = BS.packCString =<< jout ctx

-- | \( O(n) \) in the array size
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
            let mult = jTypeWidth ty'
            let resBytes = mult * intRank
            res <- mallocForeignPtrBytes resBytes
            let arrSz = mult * fromIntegral (product shape')
            withForeignPtr res $ \r' -> do
                d' <- peek d
                copyArray r' d' arrSz
            pure $ JAtom ty' shape' res

data JAtom = JAtom !JType ![CLLong] !(ForeignPtr CChar)

-- | J data backed by repa array
data JData sh = JIntArr !(R.Array RF.F sh CLLong)
              | JDoubleArr !(R.Array RF.F sh CDouble)
              | JComplexArr !(R.Array RF.F sh (Complex CDouble))
              | JBoolArr !(R.Array RF.F sh CChar)
              | JString !BS.ByteString

-- | \( O(n) \) in the array size
setJData :: (R.Shape sh) => JEnv -> BS.ByteString -- ^ Name
                         -> JData sh -> IO CInt
setJData (JEnv ctx _ _ _ jset) name (JIntArr iarr) = BS.useAsCStringLen name $ \(n, sz) -> do
    (ds, d) <- repaArr JInteger iarr
    jset ctx (fromIntegral sz) n ds d
setJData (JEnv ctx _ _ _ jset) name (JDoubleArr iarr) = BS.useAsCStringLen name $ \(n, sz) -> do
    (ds, d) <- repaArr JDouble iarr
    jset ctx (fromIntegral sz) n ds d
setJData (JEnv ctx _ _ _ jset) name (JComplexArr iarr) = BS.useAsCStringLen name $ \(n, sz) -> do
    (ds, d) <- repaArr JComplex iarr
    jset ctx (fromIntegral sz) n ds d
setJData (JEnv ctx _ _ _ jset) name (JBoolArr iarr) = BS.useAsCStringLen name $ \(n, sz) -> do
    (ds, d) <- repaArr JBool iarr
    jset ctx (fromIntegral sz) n ds d
setJData (JEnv ctx _ _ _ jset) name (JString bs) = BS.useAsCStringLen name $ \(n, sz) -> do
    (ds, d) <- strArr bs
    jset ctx (fromIntegral sz) n ds d

-- | Return a @'Ptr' ()@ suitable to be passed to @JSetA@
--
-- To be used on integer, double, and complex arrays
repaArr :: (R.Shape sh, Storable e) => JType -> R.Array RF.F sh e -> IO (CLLong, Ptr ())
repaArr jty arr = do
    let (rank', sh) = repaSize arr
        sz = product sh
    let wid = 32 + (fromIntegral $ jTypeWidth jty) * (rank' + sz)
    ptr <- mallocBytes (fromIntegral wid)
    pokeByteOff ptr 0 (227 :: CLLong) -- I think this is because it's non-boxed
    pokeByteOff ptr (sizeOf (undefined :: CLLong)) (jTypeToInt jty)
    pokeByteOff ptr (2 * sizeOf (undefined :: CLLong)) sz
    pokeByteOff ptr (3 * sizeOf (undefined :: CLLong)) rank'
    let dimOff = 4 * sizeOf (undefined :: CLLong)
    pokeArray (ptr `plusPtr` dimOff) sh
    let dataOff = dimOff + fromIntegral rank' * sizeOf (undefined :: CLLong)
    withForeignPtr (RF.toForeignPtr arr) $ \src ->
        copyArray (ptr `plusPtr` dataOff) src (fromIntegral sz)
    pure (wid, ptr)

strArr :: BS.ByteString -> IO (CLLong, Ptr ())
strArr bs = do
    let len = BS.length bs
        wid = 40 + 8 * (1 + len `div` 8)
        len' = fromIntegral len :: CLLong
    ptr <- mallocBytes wid
    pokeByteOff ptr 0 (227 :: CLLong)
    pokeByteOff ptr (sizeOf (undefined :: CLLong)) (jTypeToInt JChar)
    pokeByteOff ptr (2 * sizeOf (undefined :: CLLong)) len'
    pokeByteOff ptr (3 * sizeOf (undefined :: CLLong)) (1 :: CLLong)
    pokeByteOff ptr (4 * sizeOf (undefined :: CLLong)) len'
    let dataOff = 5 * sizeOf (undefined :: CLLong)
    BS.useAsCString bs $ \pSrc ->
        copyArray (ptr `plusPtr` dataOff) pSrc len
    pure (fromIntegral wid, ptr)

repaSize :: (R.Source r e, R.Shape sh) => R.Array r sh e -> (CLLong, [CLLong])
repaSize arr = let sh = R.extent arr in (fromIntegral $ R.rank sh, fromIntegral <$> R.listOfShape sh)

-- | J types
data JType = JBool
           | JChar
           | JInteger
           | JDouble
           | JComplex

intToJType :: CLLong -> JType
intToJType 1  = JBool
intToJType 2  = JChar
intToJType 4  = JInteger
intToJType 8  = JDouble
intToJType 16 = JComplex
intToJType _  = error "Unsupported type!"

jTypeToInt :: JType -> CLLong
jTypeToInt JBool    = 1
jTypeToInt JChar    = 2
jTypeToInt JInteger = 4
jTypeToInt JDouble  = 8
jTypeToInt JComplex = 16

jTypeWidth :: JType -> Int
jTypeWidth JBool    = sizeOf (undefined :: CChar)
jTypeWidth JChar    = sizeOf (undefined :: CChar)
jTypeWidth JInteger = sizeOf (undefined :: CLLong)
jTypeWidth JDouble  = sizeOf (undefined :: CDouble)
jTypeWidth JComplex = sizeOf (undefined :: Complex CDouble)

jData :: R.Shape sh => JAtom -> JData sh
jData (JAtom JInteger sh fp) = JIntArr $ RF.fromForeignPtr (R.shapeOfList $ fmap fromIntegral sh) (castForeignPtr fp)
jData (JAtom JDouble sh fp)  = JDoubleArr $ RF.fromForeignPtr (R.shapeOfList $ fmap fromIntegral sh) (castForeignPtr fp)
jData (JAtom JComplex sh fp) = JComplexArr $ RF.fromForeignPtr (R.shapeOfList $ fmap fromIntegral sh) (castForeignPtr fp)
jData (JAtom JBool sh fp)    = JBoolArr $ RF.fromForeignPtr (R.shapeOfList $ fmap fromIntegral sh) (castForeignPtr fp)
jData (JAtom JChar [l] fp)   = JString $ BS.fromForeignPtr (castForeignPtr fp) 0 (fromIntegral l)
jData (JAtom JChar _ _)      = error "Not supported."
