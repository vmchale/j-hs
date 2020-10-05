module Main ( main ) where

import           Data.Functor     (void)
import           Foreign.C.String (withCString)
import           Language.J

main :: IO ()
main = do
    (JEnv ctx jdo) <- jinit
    withCString "print =: monad : 'y (1!:2) 2';a =: 6?6;print a" $
        jdo ctx
