module Data.IntMap.Hack.Tool where


import Data.IntMap.Internal as I


data Action = Add | Del deriving (Show,Read,Enum)
data Task = Task Action Int deriving Show

run :: IntMap Int -> String -> IntMap Int
run im inst =
  go im (decode inst)
  where
    go imt [] = imt
    go imt (Task Add value :tasks) = go (insert value value imt) tasks
    go imt (Task Del value :tasks) = go (delete value       imt) tasks

decode :: String -> [Task]
decode [] = []
decode ('A':inst) = Task Add val : decode next
  where
    (now,next) = break (\x -> x == 'A' || x == 'D') inst
    val = read now :: Int
decode ('D':inst) = Task Del val : decode next
  where
    (now,next) = break (\x -> x == 'A' || x == 'D') inst
    val = read now :: Int
decode str = error $ "[ERROR] decode: imparsable instructions - " ++ str

double :: [Key] -> IntMap Key
double = fromList . Prelude.map (\x -> (x,x))

absList, nabsList :: [Int] -> [Int]
absList = Prelude.map abs
nabsList = Prelude.map (negate . abs)
