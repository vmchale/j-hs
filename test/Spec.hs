{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Data.Array.Repa       as R
import qualified Data.ByteString       as BS
import           Foreign.C.Types       (CDouble)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Storable      (peek)
import           Language.J
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = do
    jenv <- jinit libLinux
    defaultMain $
        testGroup "J dl"
            [ testCase "Performs calculation and has sensible output" (jComp jenv)
            , testCase "Reads back type in the environment" (jType jenv)
            ]

jComp :: JEnv -> Assertion
jComp jenv = do
    bsDispatch jenv "harmonic =: (+/ % #) &.: %"
    bsDispatch jenv "a =: harmonic 1 3 6"
    res <- jData <$> getAtomInternal jenv "a"
    doubleList res @?= [2.0]

doubleList :: JData Z -> [CDouble]
doubleList (JDoubleArr arr) = R.toList arr

jType :: JEnv -> Assertion
jType jenv@(JEnv ctx _ jget _) = do
    bsDispatch jenv "a =: 6?6"
    res <- getAtomInternal jenv "a"
    shape res @?= [6]
