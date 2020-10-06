{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Data.Array.Repa  as R
import qualified Data.ByteString  as BS
import           Foreign.C.Types  (CDouble)
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
            , testCase "Reads a string" (jStr jenv)
            ]

jStr :: JEnv -> Assertion
jStr jenv = do
    bsDispatch jenv "str =: 'hello'"
    res <- getJData jenv "str"
    unwrapStr res @?= "hello"

jComp :: JEnv -> Assertion
jComp jenv = do
    bsDispatch jenv "harmonic =: (+/ % #) &.: %"
    bsDispatch jenv "a =: harmonic 1 3 6"
    res <- getJData jenv "a"
    doubleList res @?= [2.0]

unwrapStr :: JData Z -> BS.ByteString
unwrapStr (JString bs) = bs
unwrapStr _            = error "Test suite error."

doubleList :: JData Z -> [CDouble]
doubleList (JDoubleArr arr) = R.toList arr
doubleList _                = error "Test suite failure!"

jType :: JEnv -> Assertion
jType jenv = do
    bsDispatch jenv "a =: 6?6"
    res <- getAtomInternal jenv "a"
    shape res @?= [6]
