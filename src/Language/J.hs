module Language.J ( J
                  , JEnv (..)
                  , mkJDo
                  , mkJInit
                  , jinit
                  , JType (..)
                  , intToJType
                  ) where

import           Foreign.C.String           (CString)
import           Foreign.C.Types            (CInt (..), CLLong (..))
import           Foreign.Ptr                (FunPtr, Ptr)
-- TODO: windows support
import           System.Posix.DynamicLinker (RTLDFlags (RTLD_LAZY), dlopen,
                                             dlsym)

data J

data JEnv = JEnv { context   :: Ptr J
                 , evaluator :: JDoType
                 , reader    :: JGetMType
                 -- TODO: jgetb jor whatever
                 }

type JDoType = Ptr J -> CString -> IO CInt
type JGetMType = Ptr J -> CString -> Ptr CLLong -> Ptr CLLong -> Ptr CLLong -> Ptr CLLong -> IO CInt

-- FIXME: check for error?

foreign import ccall "dynamic" mkJDo :: FunPtr JDoType -> JDoType
foreign import ccall "dynamic" mkJInit :: FunPtr (IO (Ptr J)) -> IO (Ptr J)
foreign import ccall "dynamic" mkJGetM :: FunPtr JGetMType -> JGetMType

-- only works on linux
jinit :: IO JEnv
jinit = do
    libj <- dlopen "/usr/lib/x86_64-linux-gnu/libj.so" [RTLD_LAZY]
    jt <- mkJInit =<< dlsym libj "JInit"
    let jeval = mkJDo <$> dlsym libj "JDo"
    let jread = mkJGetM <$> dlsym libj "JGetM"
    JEnv jt <$> jeval <*> jread


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
