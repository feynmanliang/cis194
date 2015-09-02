{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-type-defaults #-}
module LogAnalysis where

import Log
import Test.HUnit

parseMessage :: String -> LogMessage
parseMessage line =
  let
    splitLine = words line
    secondNum = (read (head $ tail splitLine) :: Int)
    rest = tail $ tail splitLine
  in case head line of
       'I' -> LogMessage Info  secondNum (unwords rest)
       'W' -> LogMessage Warning secondNum (unwords rest)
       'E' -> LogMessage
                (Error secondNum)
                (read (head rest) :: Int)
                (unwords $ tail rest)
       _ -> Unknown line


cl' :: IO Counts
cl' = runTestTT (TestList [
  parseMessage "E 2 562 help help" ~?= LogMessage (Error 2) 562 "help help",
  parseMessage "I 29 la la la" ~?= LogMessage Info 29 "la la la",
  parseMessage "This is not in the right format" ~?= Unknown "This is not in the right format"
  ])

parse :: String -> [LogMessage]
parse logFile = map parseMessage (lines logFile)

insert :: LogMessage -> MessageTree -> MessageTree
insert message@(LogMessage _ time _) (Node l m r) =
  let LogMessage _ nodeTime _ = m
      in if (time < nodeTime)
           then Node (insert message l) m r
           else Node l m (insert message r)
insert message@(LogMessage _ _ _) Leaf = Node Leaf message Leaf
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build messages = foldr insert Leaf messages

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = (inOrder l) ++ [m] ++ (inOrder r)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = let sortedMessages = inOrder (build messages)
                             in map (\m -> case m of
                                             LogMessage _ _ message -> message
                                             _ -> "")
                                    $ filter (\m -> case m of
                                                      LogMessage (Error severity) _ _ | severity > 50 -> True
                                                      _ -> False) sortedMessages
