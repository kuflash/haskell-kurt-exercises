import Control.Monad
import Data.Array.Base (UArray, bounds, listArray, readArray, thaw, writeArray)
import Data.Array.ST (runSTUArray)

myData :: UArray Int Int
myData = listArray (0, 5) [7, 6, 4, 8, 10, 2]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray =
  runSTUArray $ do
    stArray <- thaw myArray
    let end = (snd . bounds) myArray
    forM_ [1 .. end] $ \i -> do
      forM_ [0 .. (end - 1)] $ \j -> do
        val <- readArray stArray j
        nextVal <- readArray stArray (j + 1)
        let outOfOrder = val > nextVal
        when outOfOrder $ do
          writeArray stArray j nextVal
          writeArray stArray (j + 1) val
    return stArray
