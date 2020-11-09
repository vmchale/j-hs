{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Control.Applicative ((<$>))
import qualified Data.Array.Repa     as R
import qualified Data.ByteString     as BS
import           Foreign.C.Types     (CDouble, CInt)
import           Language.J
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = do
#ifdef linux_HOST_OS
    jenv <- jinit libLinux
#else
#ifdef darwin_HOST_OS
    jenv <- jinit (libMac [8,0,7])
#else
#ifdef mingw32_HOST_OS
    jenv <- jinit (libWindows [9,0,1])
#endif
#endif
#endif
    defaultMain $
        testGroup "J dl"
            [ testCase "Performs calculation and has sensible output" (jComp jenv)
            , testCase "Reads back type in the environment" (jType jenv)
            , testCase "Reads a string" (jStr jenv)
            , testCase "Sends an array to J" (jSetA jenv)
            , testCase "Uses J to perform a complex calculation" (regress jenv)
            , testCase "Writes strings to J values" (stringRoundtrip jenv)
            -- , testCase "Uses J for something Haskell would have a hard time with" (fill jenv)
            , testCase "Loads a library" (loadNoFail jenv)
            , testCase "Sends an integer array" (printRes jenv)
            ]

loadNoFail :: JEnv -> Assertion
loadNoFail jenv = do
#ifdef linux_HOST_OS
    jLoad jenv (linuxProfile "9.01")
#else
#ifdef darwin_HOST_OS
    jLoad jenv (macProfile [8,0,7])
#else
#ifdef mingw32_HOST_OS
    jLoad jenv (windowsProfile [9,0,1])
#endif
#endif
#endif
    bsDispatch jenv "load'tables/csv'"
    res <- bsOut jenv
    assertBool "Doesn't fail" $
        not ("error: " `BS.isInfixOf` res)

printRes :: JEnv -> Assertion
printRes jenv = do
    setJData jenv "plot_data" $ JIntArr $ R.copyS $ R.map (fromIntegral :: Int -> CInt) $ R.fromListUnboxed (R.ix1 3) [1,10,2]
    bsDispatch jenv "plot_data"
    res <- bsOut jenv
    res @?= "1 10 2"

fill :: JEnv -> Assertion
fill jenv = do
    bsDispatch jenv "random_res =: ? 70 70 $ 1e10"
    res <- getJData jenv "random_res"
    extrExtent res @?= [70, 70]
    where extrExtent :: JData R.DIM2 -> [Int]
          extrExtent (JIntArr res) = R.listOfShape $ R.extent res

regress :: JEnv -> Assertion
regress jenv = do
    let hsArr0 = R.fromListUnboxed (R.ix1 3) [1.0,2.0,3.0]
        hsArr1 = R.fromListUnboxed (R.ix1 3) [2.0,4.0,6.0]
    setJData jenv "xs" (JDoubleArr $ R.copyS $ R.map (realToFrac :: Double -> CDouble) hsArr0)
    setJData jenv "ys" (JDoubleArr $ R.copyS $ R.map (realToFrac :: Double -> CDouble) hsArr1)
    bsDispatch jenv "reg_result =: ys %. xs ^/ i.2"
    res <- getJData jenv "reg_result"
    doubleVect res @?= [5.995204332975845e-15,1.9999999999999971]

stringRoundtrip :: JEnv -> Assertion
stringRoundtrip jenv = do
    setJData jenv "stringy_string" (JString "hello" :: JData R.Z)
    res <- unwrapStr <$> getJData jenv "stringy_string"
    res @?= "hello"

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
    bsDispatch jenv "c =: harmonic 1 3 6"
    res <- getJData jenv "c"
    doubleScalar res @?= [2.0]

unwrapStr :: JData R.Z -> BS.ByteString
unwrapStr (JString bs) = bs

doubleVect :: JData R.DIM1 -> [CDouble]
doubleVect (JDoubleArr arr) = R.toList arr

doubleScalar :: JData R.Z -> [CDouble]
doubleScalar (JDoubleArr arr) = R.toList arr

intList :: JData R.DIM1 -> [CInt]
intList (JIntArr arr) = R.toList arr

jType :: JEnv -> Assertion
jType jenv = do
    bsDispatch jenv "a =: 6?6"
    res <- getJData jenv "a"
    length (intList res) @?= 6
