cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine fn l1 l2 = zipWith fn newL1 cycledL2
  where
    nToAdd = length l2
    repeatedL1 = map (take nToAdd . repeat) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

newtype Events = Events [String]

newtype Probs = Probs [Double]

instance Semigroup Events where
  (<>) events1 (Events []) = events1
  (<>) (Events []) events2 = events2
  (<>) (Events events1) (Events events2) = Events (cartCombine combiner events1 events2)
    where
      combiner = (\x y -> mconcat [x, "-", y])

instance Monoid Events where
  mempty = Events []
  mappend = (<>)

instance Semigroup Probs where
  (<>) probs1 (Probs []) = probs1
  (<>) (Probs []) probs2 = probs2
  (<>) (Probs probs1) (Probs probs2) = Probs (cartCombine (*) probs1 probs2)

instance Monoid Probs where
  mempty = Probs []
  mappend = (<>)

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable (Events events) (Probs probs) = PTable (Events events) (Probs normalizedProbs)
  where
    totalProbs = sum probs
    normalizedProbs = map (\x -> x / totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable (Events events) (Probs probs)) = mconcat pairs
    where
      pairs = zipWith showPair events probs

instance Semigroup PTable where
  (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1
  (<>) (PTable (Events []) (Probs [])) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable events probs
    where
      events = e1 <> e2
      probs = p1 <> p2

instance Monoid PTable where
  mempty = PTable (Events []) (Probs [])
  mappend = (<>)

coin :: PTable
coin = createPTable (Events ["back", "front"]) (Probs [0.5, 0.5])

roulette :: PTable
roulette = createPTable (Events ["red", "blue", "green"]) (Probs [0.1, 0.2, 0.7])
