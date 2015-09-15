{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module JoinList where

import Data.Monoid
import Buffer
import Editor
import Scrabble
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

{- Q1 -}
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ b = b
a +++ Empty = a
(+++) a b = Append (tag a <> tag b) a b

instance Monoid m => Monoid (JoinList m a) where
  mempty = Empty
  mappend = (+++)


{- Q2 -}
jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
jlSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n (Single _ a)
  | n == 0 = Just a
  | otherwise = Nothing
indexJ n (Append _ a b)
  | n < sz_a = indexJ n a
  | otherwise = indexJ (n - sz_a) b
  where sz_a = jlSize a

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl | n <= 0 = jl
dropJ _ (Single _ _) = Empty
dropJ n (Append _ a b)
  | n >= sz_a = dropJ (n - sz_a) b
  | otherwise = dropJ n a +++ b
  where sz_a = jlSize a

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n _ | n <= 0 = Empty
takeJ _ jl@(Single _ _) = jl
takeJ n (Append _ a b)
  | n > sz_a = a +++ takeJ (n - sz_a) b
  | otherwise = takeJ n a
  where sz_a = jlSize a

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

{- Q4 -}
toList :: JoinList m a -> [a]
toList Empty = []
toList (Single _ a) = [a]
toList (Append _ l1 l2) = toList l1 ++ toList l2

instance Buffer (JoinList (Score, Size) String) where
  toString = init . unlines . toList
  fromString = mconcat . map convertLine . lines
    where convertLine l = Single (scoreString l, 1) l
  line = indexJ
  replaceLine n ln old = let prefix = takeJ n old
                             suffix = dropJ (n+1) old
                          in prefix +++ fromString ln +++ suffix
  numLines = jlSize
  value = getScore . fst . tag





main ::  IO ()
main = runEditor editor ((fromString . unlines $
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ])::JoinList (Score, Size) String)
