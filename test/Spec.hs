{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import qualified Data.ByteString       as BS
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Storable      (peek)
import           Language.J
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = do
    jenv <- jinitLinux
    defaultMain $
        testGroup "J dl"
            [ testCase "Performs calculation and has sensible output" (jComp jenv)
            , testCase "Reads back type in the environment" (jType jenv)
            ]

jComp :: JEnv -> Assertion
jComp (JEnv ctx jdo _ jout) = do
    BS.useAsCString "harmonic =: (+/ % #) &.: %" $
        jdo ctx
    BS.useAsCString "harmonic 1 3 6" $
        jdo ctx
    res <- BS.packCString =<< jout ctx
    res @?= "2\n"

jType :: JEnv -> Assertion
jType (JEnv ctx jdo jget _) = do
    BS.useAsCString "a =: 6?6" $
        jdo ctx
    BS.useAsCString "a" $ \a ->
        alloca $ \t ->
        alloca $ \s ->
        alloca $ \r ->
        alloca $ \d -> do
            jget ctx a t s r d
            t' <- peek t
            intToJType t' @?= JInteger
