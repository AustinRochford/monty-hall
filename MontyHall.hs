{-# LANGUAGE TupleSections #-}

import Control.Applicative ((<$>))

import Data.List (maximumBy, (\\))
import Data.Ord (comparing)

import Numeric.Probability.Distribution (decons, just, T, uniform, (>>=?))

data Door = First | Second | Third deriving (Show, Eq, Ord)

prior :: (Fractional p) => T p Door
prior = uniform [First, Second, Third]

open :: (Fractional p) => Door -> Door -> T p Door
open chosen correct = uniform $ otherDoors [chosen, correct]

posterior :: (Fractional p) => Door -> Door -> T p Door
posterior chosen opened = fst <$> (prior >>+ open chosen >>=? just opened . snd)

--door utilities
otherDoors :: [Door] -> [Door]
otherDoors = (\\) [First, Second, Third]

--distribution utilities
graph :: (Num p, Ord b, Ord a) => (a -> T p b) -> (a -> T p (a, b))
graph f x = (x,) <$> f x

(>>+) :: (Num p, Ord b, Ord a) => T p a -> (a -> T p b) -> T p (a, b)
dist >>+ f = dist >>= graph f

--miscellaneous utilities
infixr 8 ##
(##) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(##) = (.) . (.)
