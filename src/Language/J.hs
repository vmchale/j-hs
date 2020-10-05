module Language.J ( J
                  , JEnv (..)
                  , mkJDo
                  , mkJInit
                  , jinit
                  ) where

import           Foreign.C.String           (CString)
import           Foreign.Ptr                (FunPtr, Ptr)
-- TODO: windows support
import           System.Posix.DynamicLinker (RTLDFlags (RTLD_LAZY), dlopen,
                                             dlsym)

data J

data JEnv = JEnv { context   :: Ptr J
                 , evaluator :: Ptr J -> CString -> IO ()
                 -- TODO: jgetb or whatever
                 }

foreign import ccall "dynamic" mkJDo :: FunPtr (Ptr J -> CString -> IO ()) -> Ptr J -> CString -> IO ()
foreign import ccall "dynamic" mkJInit :: FunPtr (IO (Ptr J)) -> IO (Ptr J)

-- only works on linux
jinit :: IO JEnv
jinit = do
    libj <- dlopen "/usr/lib/x86_64-linux-gnu/libj.so" [RTLD_LAZY]
    jt <- mkJInit =<< dlsym libj "JInit"
    let jeval = mkJDo <$>  dlsym libj "JDo"
    JEnv jt <$> jeval
