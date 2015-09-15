{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

{- Q1 -}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0..]]

{- Q2 -}
fibs2 :: [Integer]
fibs2 = map fst $ scanl
    (\(p2, p1) _ -> (p1, p2 + p1))
    (0, 1)
    [0::Integer ..]

{- Q3 -}
data Stream a = a `Cons` Stream a

instance (Show a) => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

{- Q4 -}
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) (streamMap f b)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed next seed = Cons seed (streamFromSeed next (next seed))

{- Q5 -}
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleave :: Stream a -> Stream a -> Stream a
interleave (Cons x xs) (Cons y ys) = Cons x (Cons y (interleave xs ys))

ruler :: Stream Integer
ruler = interleave (streamRepeat 0) (streamMap maxPow2EvenDiv (streamFromSeed (+2) 2))
  where
    maxPow2EvenDiv :: Integer -> Integer
    maxPow2EvenDiv n = maximum [pow | pow <- [1..intLog2 n], n `mod` 2^pow == 0]

    intLog2 :: Integer -> Integer
    intLog2 x = floor (logBase 2 (fromIntegral x)::Double)

{- Q6 -}
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap (*(-1))
  (+) (Cons a as) (Cons b bs) = Cons (a+b) (as+bs)
  (*) (Cons a as) (Cons b bs) = Cons (a*b) ((streamMap (*a) bs) + (as * (Cons b bs)))

instance Fractional (Stream Integer) where
  (/) (Cons a as) (Cons b bs) = q
    where q = Cons (a `div` b) (streamMap (`div` b) (as - (q * bs)))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^(2::Integer))

{- Q7 -}
data Matrix = Matrix (Integer, Integer, Integer, Integer)

instance Num Matrix where
  (*) (Matrix (a,b,c,d)) (Matrix (e,f,g,h)) = Matrix (a*e + b*g, a*f + b*h, c*e + d*g, c*f + d*h)

fib4 :: Integer -> Integer
fib4 n = case (Matrix (1, 1, 1, 0))^n of
           (Matrix (_,_,fn,_)) -> fn

