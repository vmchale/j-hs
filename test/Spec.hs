{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import qualified Data.Array.Repa  as R
import qualified Data.ByteString  as BS
import           Foreign.C.Types  (CDouble, CInt)
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
            , testCase "Sends an array to J" (jSetA jenv)
            , testCase "Uses J to perform a complex calculation" (regress jenv)
            ]

regress :: JEnv -> Assertion
regress jenv = do
    let hsArr0 = R.fromListUnboxed (R.ix1 3) [1.0,2.0,3.0]
        hsArr1 = R.fromListUnboxed (R.ix1 3) [2.0,4.0,6.0]
    setJData jenv "xs" (JDoubleArr $ R.copyS $ R.map (realToFrac :: Double -> CDouble) hsArr0)
    setJData jenv "ys" (JDoubleArr $ R.copyS $ R.map (realToFrac :: Double -> CDouble) hsArr1)
    bsDispatch jenv "reg_result =: ys %. xs ^/ i.2"
    res <- getJData jenv "reg_result"
    doubleVect res @?= [5.995204332975845e-15,1.9999999999999971]

jSetA :: JEnv -> Assertion
jSetA jenv = do
    let hsArr = R.fromListUnboxed (R.ix1 3) [1,3,6]
    setJData jenv "b" (JIntArr $ R.copyS $ R.map (fromIntegral :: Int -> CInt) hsArr)
    res <- getJData jenv "b"
    intList res @?= [1,3,6]

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
    doubleScalar res @?= [2.0]

unwrapStr :: JData R.Z -> BS.ByteString
unwrapStr (JString bs) = bs
unwrapStr _            = error "Test suite error."

doubleVect :: JData R.DIM1 -> [CDouble]
doubleVect (JDoubleArr arr) = R.toList arr
doubleVect _                = error "Test suite failure!"

doubleScalar :: JData R.Z -> [CDouble]
doubleScalar (JDoubleArr arr) = R.toList arr
doubleScalar _                = error "Test suite failure!"

intList :: JData R.DIM1 -> [CInt]
intList (JIntArr arr) = R.toList arr
intList _             = error "Test suite failure!"

jType :: JEnv -> Assertion
jType jenv = do
    bsDispatch jenv "a =: 6?6"
    res <- getJData jenv "a"
    length (intList res) @?= 6
