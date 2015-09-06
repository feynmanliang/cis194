{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

module HW4 where

import Data.List
import Test.HUnit

{-Q1-}
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum
      . filter even
      . takeWhile (/= 1)
      . iterate (\n -> if even n then n `div` 2 else 3*n + 1)

{-Q2-}
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where
    height Leaf             = -1
    height (Node h _ _ _) = h

    count Leaf = 0
    count (Node _ lt _ rt) = count lt + 1 + count rt

    insert x Leaf             = Node 0 Leaf x Leaf
    insert x (Node h Leaf r Leaf) = Node (h+1) (insert x Leaf) r Leaf
    insert x (Node h Leaf r rt) = Node h (insert x Leaf) r rt
    insert x (Node h lt r Leaf) = Node h lt r (insert x Leaf)
    insert x (Node h lt r rt)
      | count lt > count rt   = Node h lt r (insert x rt)
      | count lt < count rt   = Node h (insert x lt) r rt
      | otherwise             = let newlt = insert x lt
                                    newheight = 1 + max (height newlt) (height rt)
                                in Node newheight newlt r rt

{-Q3-}
xor :: [Bool] -> Bool
xor = odd . length . filter (id)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f(x) : xs) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x acc y -> acc (y `f` x)) id xs base

-- foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
-- foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn

{-Q4: Sieve of Sundaram -}
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2 * x + 1) . (`sub` ijs) $ [1..n]
    where
        ijs = takeWhile (<= n) . map head . group . sort $ [i + j + 2*i*j | j <- [1..n], i <- [1.. (n - j) `div` (1 + 2*j)]]

        sub [] _  = []
        sub xs [] = xs
        sub xxs@(x:xs) yys@(y:ys)
          | x < y     = x : sub xs yys
          | x == y    = sub xs ys
          | otherwise = sub xxs ys

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

cl :: IO Counts
cl = runTestTT (TestList [
  cartProd [1,2] ['a','b'] ~?= [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
  ])
