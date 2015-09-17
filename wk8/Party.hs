{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Party where

import Data.List
import Data.Tree

import Employee

{- Q1 -}
glCons :: Employee -> GuestList -> GuestList
glCons employee (GL employees totFun) =
  GL (employee:employees) (empFun employee + totFun)

-- orphan-instance is type class instance C T defiend in module distinct
-- from both modules where C and T are defined
instance Monoid GuestList where
  mempty = GL [] 0
  (GL e1 f1) `mappend` (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

{- Q2 -}
treeFold :: (a -> [b] -> b) -> (a -> b) -> Tree a -> b
treeFold combOp leafOp tree =
  case tree of
    Node label [] -> leafOp label
    Node label children -> combOp label (map (treeFold combOp leafOp) children)

{- Q3 -}
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss subTrees = (bestWithBoss, bestWithoutBoss)
  where bestWithBoss = boss `glCons` foldl1' mappend (map snd subTrees)
        bestWithoutBoss = foldl1' mappend (map fst subTrees)

{- Q4 -}
maxFun :: Tree Employee -> GuestList
maxFun tree = maximum $ treeFold nextLevel makeGuestList tree
  where makeGuestList emp = (GL [emp] (empFun emp), mempty)

{- Q5 -}
formatOutput :: GuestList -> String
formatOutput (GL employees fun) =
  "Total fun: " ++ show fun ++ "\n" ++ unlines (sort $ map empName employees)

main :: IO ()
main = do contents <- readFile "company.txt"
          let companyTree = read contents :: Tree Employee
              bestGuestList = maxFun companyTree
          putStrLn $ formatOutput bestGuestList

