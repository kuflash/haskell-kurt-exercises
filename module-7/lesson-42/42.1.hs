import Control.Monad
import Data.Array.Base (UArray, bounds, listArray, readArray, thaw, writeArray, (!))
import Data.Array.ST (runSTUArray)

firstArray :: UArray Int Int
firstArray = listArray (0, 4) $ repeat 1

secondArray :: UArray Int Int
secondArray = listArray (0, 4) $ repeat 0

cross :: (UArray Int Int, UArray Int Int) -> Int -> UArray Int Int
cross (fArray, sArray) point =
  runSTUArray $ do
    fSTArray <- thaw fArray
    let end = (snd . bounds) fArray
    forM_ [point .. end] $ \i -> do
      writeArray fSTArray i $ sArray ! i
    return fSTArray
