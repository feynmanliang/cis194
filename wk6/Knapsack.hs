{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

module Knapsack where

import Data.Array

{-This example leverages the laziness of `Data.Array.IArray array` to implement     -}
{-dynamic programming. Lazy evaluation is used to work out the proper order in which-}
{-to compute the cells, which depend on previous comuptations.                      -}

knapsack01 :: [Double]  -- values
           -> [Integer] -- weights >= 0
           -> Integer   -- knapsack size
           -> Double    -- value of best knapsack
knapsack01 vs ws maxW = m!(numItems-1, maxW)
  where numItems = length vs
        m = array ((-1,0), (numItems-1, maxW)) $
          [((-1,w), 0) | w <- [0 .. maxW]] ++ -- base case, using zero items => 0 value
          [((i,0), 0) | i <- [0 .. numItems-1]] ++ -- base case, using zero weights => 0 value
          [((i,w), best)
              | i <- [0 .. numItems-1]
              , w <- [1 .. maxW]
              , let best | ws!!i > w = m!(i-1, w) -- weight of item i too big for remaining space
                         | otherwise = max (m!(i-1, w)) -- don't add item i
                                           (m!(i-1, w - ws!!i) + vs!!i) -- add item i
          ]

