module Chess.ComputerPlayer where

import qualified Data.Map as Map
import Data.List (sortBy)
import Chess.Color
import Chess.Figure
import Chess.Field
import Chess.Move
import Chess.FigureMoves
import Chess.Game
import Chess.Rank

-- | Returns a sequence of the best ranked moves.
moves :: Game -> [Game]
moves game =
  let moves = validGames game
  in if null moves
     then []
     else let
            rankedMoves = map (\g -> (g, rank g (gameColor g))) moves
            rankedMovesSorted = sortBy (\(_,rank1) (_,rank2) -> compare rank1 rank2) rankedMoves
            firstRank = snd . head $ rankedMovesSorted
            maxRankMoves = takeWhile (\(_,rank) -> rank == firstRank) rankedMovesSorted
          in map fst maxRankMoves

-- | Makes a move and returns the next game state.
makeMove :: Game -> Maybe Game
makeMove game = case moves game of
  [] -> Nothing
  h:_ -> Just h

{-

ghci
:load Chess/ComputerPlayer.hs
let Just g1 = move (Field 7 2) (Field 7 4) Nothing GameStart
let Just g2 = move (Field 5 7) (Field 5 6) Nothing g1
let Just g3 = move (Field 6 2) (Field 6 4) Nothing g2
let Just g4 = move (Field 4 8) (Field 8 4) Nothing g3
length $ moves GameStart -- 2
length $ moves g1 -- 2
length $ moves g2 -- 2
length $ moves g3 -- 1
length $ moves g4 -- 0
let Just g1 = makeMove GameStart
let Just g2 = makeMove g1
let Just g3 = makeMove g2
let Just g4 = makeMove g3
g4
:q

-}
  
