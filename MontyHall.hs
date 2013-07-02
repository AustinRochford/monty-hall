{-# LANGUAGE TupleSections #-}

import Prelude hiding (map)
import Data.List (maximumBy, (\\))
import Data.Ord (comparing)
import Numeric.Probability.Distribution (decons, just, map, T, uniform, (>>=?))

data Door = First | Second | Third deriving (Show, Eq, Ord)

prior :: (Fractional p) => T p Door
prior = uniform [First, Second, Third]

open :: (Fractional p) => Door -> Door -> T p Door
open chosen correct = uniform $ otherDoors [chosen, correct]

joint :: (Fractional p) => Door -> Door -> T p (Door, Door)
joint chosen correct = map (correct,) $ open chosen correct

posterior :: (Fractional p) => Door -> Door -> T p Door
posterior chosen opened = map fst $ prior >>= joint chosen >>=? just opened . snd

posteriorMax :: Door -> Door -> Door
posteriorMax = fst . maximumBy (comparing snd) . decons ## posterior

change :: Door -> Door -> Bool
change chosen = (chosen ==) . posteriorMax chosen

--utilities
otherDoors :: [Door] -> [Door]
otherDoors = (\\) [First, Second, Third]

infixr 8 ##
(##) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(##) = (.) . (.)
