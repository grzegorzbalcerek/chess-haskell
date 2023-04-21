module Chess.MinMax where

import qualified Data.Map as Map
import Data.List (sortBy)
import Chess.Game

data Tree a = Node a [Tree a]
  deriving (Show)

reptree f a = Node a (map (reptree f) (f a))

gametree :: Game -> Tree Game
gametree p = reptree validGames p

maptree :: (a -> b) -> Tree a -> Tree b
maptree f (Node a []) = Node (f a) []
maptree f (Node a xs) = Node (f a) (map (maptree f) xs) 


--mapmin = map minimum

mapmin (nums:rest) = 
  let minnums = minimum nums
  in minnums : (omit minnums rest)

omit pot [] = []
omit pot (nums:rest) 
  | minleq pot nums = omit pot rest
  | otherwise = (minimum nums) : (omit (minimum nums) rest)

minleq [] pot = False
minleq (num:rest) pot = if num <= pot then True else minleq rest pot

mapmax = map maximum

maximize :: Tree Int -> Int
maximize = maximum . maximize'

maximize' (Node n []) = [n]
maximize' (Node n xs) = mapmin (map minimize' xs)

minimize :: Tree Int -> Int
minimize = minimum . minimize'

minimize' (Node n []) = [n]
minimize' (Node n xs) = mapmax (map maximize' xs)

prune :: Int -> Tree a -> Tree a
prune 0 (Node a _) = Node a []
prune n (Node a xs) = Node a (map (prune (n-1)) xs) 

