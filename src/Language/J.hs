module Language.J ( -- * Environment
                    JEnv (..)
                  , jinit
                  , jinitLinux
                  , bsDispatch
                  , bsOut
                  -- * FFI
                  , JDoType
                  , JGetMType
                  , J
                  , mkJDo
                  , mkJInit
                  , mkJGetM
                  -- * Marshaling (to Haskell)
                  , JType (..)
                  , intToJType
                  ) where

import           Control.Applicative        ((<$>), (<*>))
import           Data.ByteString            as BS
import           Data.Functor               (void)
import           Foreign.C.String           (CString)
import           Foreign.C.Types            (CInt (..), CLLong (..))
import           Foreign.Ptr                (FunPtr, Ptr)
import           System.Posix.DynamicLinker (RTLDFlags (RTLD_LAZY), dlopen,
                                             dlsym)
-- TODO: windows support
-- (https://hackage.haskell.org/package/Win32-2.10.0.0/docs/System-Win32-DLL.html#v:getProcAddress)

data J

data JEnv = JEnv { context   :: Ptr J
                 , evaluator :: JDoType
                 , reader    :: JGetMType
                 , output    :: JGetRType
                 }

type JDoType = Ptr J -> CString -> IO CInt
type JGetMType = Ptr J -> CString -> Ptr CLLong -> Ptr CLLong -> Ptr CLLong -> Ptr CLLong -> IO CInt
type JGetRType = Ptr J -> IO CString

foreign import ccall "dynamic" mkJDo :: FunPtr JDoType -> JDoType
foreign import ccall "dynamic" mkJInit :: FunPtr (IO (Ptr J)) -> IO (Ptr J)
foreign import ccall "dynamic" mkJGetM :: FunPtr JGetMType -> JGetMType
foreign import ccall "dynamic" mkJGetR :: FunPtr JGetRType -> JGetRType

jinitLinux :: IO JEnv
jinitLinux = jinit "/usr/lib/x86_64-linux-gnu/libj.so"

-- | Get a J environment
--
-- Don't pass the resultant 'JEnv' between threads; that fails for whatever
-- reason
jinit :: FilePath -- ^ Path to J library
      -> IO JEnv
jinit libFp = do
    libj <- dlopen libFp [RTLD_LAZY]
    jt <- mkJInit =<< dlsym libj "JInit"
    let jeval = mkJDo <$> dlsym libj "JDo"
    let jread = mkJGetM <$> dlsym libj "JGetM"
    let jOut = mkJGetR <$> dlsym libj "JGetR"
    JEnv jt <$> jeval <*> jread <*> jOut

-- | Send some J code to the environment.
--
-- @20?20@
bsDispatch :: JEnv -> BS.ByteString -> IO ()
bsDispatch (JEnv ctx jdo _ _) bs =
    void $ BS.useAsCString bs $ jdo ctx

bsOut :: JEnv -> IO BS.ByteString
bsOut (JEnv ctx _ _ jout) = BS.packCString =<< jout ctx

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
intToJType _  = error "Bad or unknown type!"
