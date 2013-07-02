import Numeric.Probability.Distribution (T, uniform)

data Door = First | Second | Third deriving (Show, Eq, Ord)

prior :: (Fractional p) => T p Door
prior = uniform [First, Second, Third]
