module Main ( main ) where

import           Data.Functor          (void)
import           Foreign.C.String      (withCString)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Storable      (peek)
import           Language.J
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main =
    defaultMain $
        testGroup "J dl"
            [ testCase "Reads back type in the environment" jType ]

jType :: Assertion
jType = do
    (JEnv ctx jdo jget) <- jinit
    withCString "a =: 6?6" $
        jdo ctx
    withCString "a" $ \a ->
        alloca $ \t ->
        alloca $ \s ->
        alloca $ \r ->
        alloca $ \d -> do
            jget ctx a t s r d
            t' <- peek t
            intToJType t' @?= JInteger
