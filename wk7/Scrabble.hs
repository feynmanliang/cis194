{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Maybe

{- Q3 -}
newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

letterScores :: [(Char, Int)]
letterScores = zip ['a'..'z'] [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]
  ++ zip ['A'..'Z'] [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]

score :: Char -> Score
score c = Score (fromMaybe 0 letterScore)
  where letterScore = lookup c letterScores

scoreString :: String -> Score
scoreString = mconcat . map score

getScore :: Score -> Int
getScore (Score x) = x
