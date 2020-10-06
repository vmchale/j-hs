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
    jenv <- jinit libLinux
    defaultMain $
        testGroup "J dl"
            [ testCase "Performs calculation and has sensible output" (jComp jenv)
            , testCase "Reads back type in the environment" (jType jenv)
            ]

jComp :: JEnv -> Assertion
jComp jenv = do
    bsDispatch jenv "harmonic =: (+/ % #) &.: %"
    bsDispatch jenv "harmonic 1 3 6"
    res <- bsOut jenv
    res @?= "2\n"

jType :: JEnv -> Assertion
jType jenv@(JEnv ctx _ jget _) = do
    bsDispatch jenv "a =: 6?6"
    res <- getAtomInternal jenv "a"
    -- ty res @?= JInteger
    -- rank res @?= 1
    shape res @?= [6]
