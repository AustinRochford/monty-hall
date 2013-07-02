{-# LANGUAGE TupleSections #-}

import Prelude hiding (map)
import Data.List ((\\))
import Numeric.Probability.Distribution (just, map, T, uniform, (>>=?))

data Door = First | Second | Third deriving (Show, Eq, Ord)

prior :: (Fractional p) => T p Door
prior = uniform [First, Second, Third]

open :: (Fractional p) => Door -> Door -> T p Door
open chosen correct = uniform $ otherDoors [chosen, correct]

joint :: (Fractional p) => Door -> Door -> T p (Door, Door)
joint chosen correct = map (correct,) $ open chosen correct

posterior :: (Fractional p) => Door -> Door -> T p Door
posterior chosen opened = map fst $ prior >>= joint chosen >>=? just opened . snd

otherDoors :: [Door] -> [Door]
otherDoors = (\\) [First, Second, Third]
