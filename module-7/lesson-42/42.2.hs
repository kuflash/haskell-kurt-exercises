import Control.Monad
import Data.Array.Base (UArray, bounds, listArray, readArray, thaw, writeArray, (!))
import Data.Array.ST (runSTUArray)

numbers :: UArray Int Int
numbers = listArray (0, 6) [0, 2, 1, 0, 0, 3, 0]

replaceZeros :: UArray Int Int -> UArray Int Int
replaceZeros arr =
  runSTUArray $ do
    stArray <- thaw arr
    let end = (snd . bounds) arr
    forM_ [0 .. end] $ \i -> do
      val <- readArray stArray i
      when (val == 0) $ do
        writeArray stArray i (-1)
    return stArray
