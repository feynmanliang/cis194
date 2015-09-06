{-# LANGUAGE FlexibleInstances,
             TypeSynonymInstances,
             GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

module Calc (Expr(..)) where

import ExprT
import Parser
import Test.HUnit
import qualified Data.Map.Strict as M
import qualified StackVM as S

eval :: ExprT -> Integer
eval expr = case expr of
             Lit x -> x
             Add x y -> eval x + eval y
             Mul x y -> eval x * eval y


cl1 :: IO Counts
cl1 = runTestTT (TestList [
  eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) ~?= 20
  ])

evalStr :: String -> Maybe Integer
evalStr expr = fmap eval (parseExp Lit Add Mul expr)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reifyExprT :: ExprT -> ExprT
reifyExprT = id

cl2 :: IO Counts
cl2 = runTestTT (TestList [
  (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) ~?= (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
  ])


instance Expr Integer where
  lit x = x
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>=0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

{-Q5-}

cl3 :: IO Counts
cl3 = runTestTT (TestList [
  (testExp :: Maybe Integer) ~?= Just (7),
  (testExp :: Maybe Bool) ~?= Just True,
  (testExp :: Maybe MinMax) ~?= Just (MinMax 5),
  (testExp :: Maybe Mod7) ~?= Just (Mod7 0)
  ])

instance Expr S.Program where
  lit x = [S.PushI x]
  add x y = x ++ y ++ [S.Add]
  mul x y = x ++ y ++ [S.Mul]

testVM = S.stackVM [S.PushI 3, S.PushI 5, S.Add]

{-Q6-}

class HasVars a where
  var :: String -> a

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add e1 e2 m = (+) <$> e1 m <*> e2 m
  mul e1 e2 m = (*) <$> e1 m <*> e2 m

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs


testVars = withVars [("x", 6)] $ add (lit 3) (var "x")
