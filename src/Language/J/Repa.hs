module Language.J.Repa ( intArray
                       , doubleArray
                       ) where

import qualified Data.Array.Repa                 as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import           Foreign.C.Types                 (CDouble, CInt)
import           Language.J

intArray :: R.Shape sh => JAtom -> R.Array R.D sh CInt
intArray (JAtom JInteger _ sh fp) = R.map fromIntegral $ RF.fromForeignPtr (R.shapeOfList $ fmap fromIntegral sh) fp
intArray _                        = error "Invalid type!"

doubleArray :: R.Shape sh => JAtom -> R.Array R.D sh CDouble
doubleArray (JAtom JDouble _ sh fp) = R.map realToFrac $ RF.fromForeignPtr (R.shapeOfList $ fmap fromIntegral sh) fp
doubleArray _                       = error "Invalid type!"
