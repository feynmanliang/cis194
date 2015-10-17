{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import GHC.Exts

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

-- Ex. 2
battle :: Battlefield -> Rand StdGen Battlefield
battle bf = (\(a,d) -> Battlefield { attackers = a, defenders = d}) <$> newCounts
        where
          numAttackers = attackers bf
          numDefenders = defenders bf
          attackDies = reverse . sortWith unDV <$> (sequence $ replicate (numAttackers - 1) die)
          defendDies = reverse . sortWith unDV <$> (sequence $ replicate numDefenders die)
          pairedDies = zip <$> attackDies <*> defendDies
          newCounts =
            foldl' scoreBattle (numAttackers, numDefenders) <$> pairedDies
            where scoreBattle (na, nd) (rolla, rolld)
                    | rolla > rolld = (na, nd - 1)
                    | otherwise = (na - 1, nd)

-- Ex. 3
invade :: Battlefield -> Rand StdGen Battlefield
{-invade bf = head <$> (dropWhile unitsLeft) <$> (sequence $ iterate (\m -> m >>= battle) (battle bf))-}
{-        where                                                                                       -}
{-          unitsLeft Battlefield { attackers = a, defenders = d } = (d > 0) && (a >= 2)              -}
invade bf = let result = battle bf in
  result >>= \newBf -> case newBf of
    Battlefield { attackers = a, defenders = d } | a < 2 || d == 0 -> result
    _ -> invade newBf

-- Ex. 4
successProb :: Battlefield -> Rand StdGen Double
successProb bf = (\wins -> (fromIntegral wins) / (fromIntegral numSims)) <$> numberWins
        where
          numSims = 1000
          simulations = sequence $ replicate numSims (invade bf)
          numberWins = (\sims -> length $ filter (\bf -> (defenders bf) == 0) sims) <$> simulations
