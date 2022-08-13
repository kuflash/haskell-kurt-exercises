import qualified Data.Map as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB =
  Map.fromList
    [ ("Arkhem", (42.6054, -70.7829)),
      ("Innsmoot", (42.8250, -70.8150)),
      ("Carcosa", (29.9714, -90.7694)),
      ("New-York", (40.7776, -73.9691))
    ]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat, long) = (toRadians lat, toRadians long)

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
  where
    (rlat1, rlong1) = latLongToRads coords1
    (rlat2, rlong2) = latLongToRads coords2
    dlat = rlat2 - rlat1
    dlong = rlong2 - rlong1
    a = (sin (dlat / 2)) ^ 2 + cos rlat1 * cos rlat2 * (sin (dlong / 2)) ^ 2
    c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    earthRadius = 6378.1

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, we don't found some of entered city"
printDistance (Just value) = putStrLn (show value ++ "km")

main :: IO ()
main = do
  putStrLn "Enter name of first city"
  start <- getLine
  let startCoords = Map.lookup start locationDB
  putStrLn "Enter name of second city"
  destination <- getLine
  let destinationCoords = Map.lookup destination locationDB
  let distance = haversine <$> startCoords <*> destinationCoords
  printDistance distance
