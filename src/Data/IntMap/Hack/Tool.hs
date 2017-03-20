module Data.IntMap.Hack.Tool where


import Data.IntMap.Internal as I


data Action = Add | Del deriving (Show,Read,Enum)
data Task = Task Action Int deriving Show

run im inst =
  go im (decode inst)
  where
    go im [] = im
    go im (Task Add value :tasks) = go (insert value value im) tasks
    go im (Task Del value :tasks) = go (delete value       im) tasks

decode [] = []
decode ('A':inst) = Task Add val : decode next
  where
    (now,next) = break (\x -> x == 'A' || x == 'D') inst
    val = read now :: Int
decode ('D':inst) = Task Del val : decode next
  where
    (now,next) = break (\x -> x == 'A' || x == 'D') inst
    val = read now :: Int

double = fromList . Prelude.map (\x -> (x,x))

absList, nabsList :: [Int] -> [Int]
absList = Prelude.map abs
nabsList = Prelude.map (negate . abs)
