{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Golf where

import Data.List
import Test.HUnit

skips :: [a] -> [[a]]
skips xs = map
  (`everyNth` xs)
  [1..(length xs)]

everyNth :: Int -> [a] -> [a]
everyNth n xs
  | length xs < n = []
  | otherwise = nth : everyNth n rest
                where nth : rest = drop (n-1) xs


cl :: IO Counts
cl = runTestTT (TestList [
  skips "ABCD" ~?= ["ABCD", "BD", "C", "D"],
  skips "hello!" ~?= ["hello!", "el!", "l!", "l", "o", "!"],
  skips [1] ~?= [[1]],
  skips [True,False] ~?= [[True,False], [False]]
  ])

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | x < y && y > z = y : localMaxima (y:z:xs)
  | otherwise = localMaxima (y:z:xs)
localMaxima _ = []

cl2 :: IO Counts
cl2 = runTestTT (TestList [
  localMaxima [2,9,5,6,1] ~?= [9,6],
  localMaxima [2,3,4,1,5] ~?= [4],
  localMaxima [1,2,3,4,5] ~?= []
  ])

histogram ::  [Integer] -> String
histogram xs = unlines
        . reverse
        . takeWhile (any (/= ' '))
        . transpose
        . map (\n -> show n ++ "-" ++ replicate (length (filter (== n) xs)) '*' ++ repeat ' ')
        $ [0..9]

hist ::  [Integer] -> IO ()
hist = putStrLn <$> histogram
