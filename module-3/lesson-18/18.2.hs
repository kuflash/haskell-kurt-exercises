import qualified Data.Map as Map

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

organInventory :: Map.Map Organ Int
organInventory = Map.fromList (zip allOrgans countOrgans)
  where
    allOrgans = [Heart ..]
    catalogOrgans = Map.elems organCatalog
    findAllOrgans organ = filter (== organ) catalogOrgans
    getCountOrgans = (length . findAllOrgans)
    countOrgans = map getCountOrgans allOrgans
