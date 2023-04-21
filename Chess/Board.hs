module Chess.Board where

import qualified Data.Map as Map
import Chess.Color
import Chess.Figure
import Chess.Field
import Chess.Move

type Board = Map.Map Field Figure

-- | The board state when the game starts.
startingBoard :: Board
startingBoard = Map.fromList [
  (Field 1 1, Figure Rook White),
  (Field 2 1, Figure Knight White),
  (Field 3 1, Figure Bishop White),
  (Field 4 1, Figure Queen White),
  (Field 5 1, Figure King White),
  (Field 6 1, Figure Bishop White),
  (Field 7 1, Figure Knight White),
  (Field 8 1, Figure Rook White),
  (Field 1 2, Figure Pawn White),
  (Field 2 2, Figure Pawn White),
  (Field 3 2, Figure Pawn White),
  (Field 4 2, Figure Pawn White),
  (Field 5 2, Figure Pawn White),
  (Field 6 2, Figure Pawn White),
  (Field 7 2, Figure Pawn White),
  (Field 8 2, Figure Pawn White),
  (Field 1 7, Figure Pawn Black),
  (Field 2 7, Figure Pawn Black),
  (Field 3 7, Figure Pawn Black),
  (Field 4 7, Figure Pawn Black),
  (Field 5 7, Figure Pawn Black),
  (Field 6 7, Figure Pawn Black),
  (Field 7 7, Figure Pawn Black),
  (Field 8 7, Figure Pawn Black),
  (Field 1 8, Figure Rook Black),
  (Field 2 8, Figure Knight Black),
  (Field 3 8, Figure Bishop Black),
  (Field 4 8, Figure Queen Black),
  (Field 5 8, Figure King Black),
  (Field 6 8, Figure Bishop Black),
  (Field 7 8, Figure Knight Black),
  (Field 8 8, Figure Rook Black)]

-- | Shows the board.
showBoard :: Board -> String
showBoard board = " abcdefgh\n" ++ concat (map showRow [8,7..1]) ++ " abcdefgh\n"
  where showRow row = (show row) ++
                      concat (map (showField row) [1..8]) ++
                      (show row) ++ "\n"
        showField row col = case Map.lookup (Field col row) board of
                              Just x -> show x
                              Nothing -> "."

-- | Returns a new board, updated with a move.
updateBoard :: Board -> Move -> Board
updateBoard board (RegularMove from to) =
  case Map.lookup from board of
    Just figure -> Map.insert to figure . Map.delete from $ board
    _ -> board
updateBoard board (PromotionMove from to figure) =
  case Map.lookup from board of
    Just _ -> Map.insert to figure . Map.delete from $ board
    _ -> board
updateBoard board (EnPassantMove from to captured) =
  case Map.lookup from board of
    Just figure -> Map.insert to figure .
                   Map.delete from .
                   Map.delete captured $ board
    _ -> board
updateBoard board (CastlingMove fromKing toKing fromRook toRook) =
  case (Map.lookup fromKing board, Map.lookup fromRook board) of
    (Just king, Just rook) -> Map.insert toKing king .
                              Map.insert toRook rook .
                              Map.delete fromKing .
                              Map.delete fromRook $ board
    _ -> board


{-

ghci
:load Chess/Board.hs
startingBoard
showBoard startingBoard
putStr $ showBoard startingBoard
putStr $ showBoard $ updateBoard startingBoard (RegularMove (Field 2 2) (Field 2 3))
putStr $ showBoard $ updateBoard startingBoard (RegularMove (Field 3 3) (Field 2 3))
putStr $ showBoard $ updateBoard startingBoard (PromotionMove (Field 2 2) (Field 2 8) (Figure Queen White))
putStr $ showBoard $ updateBoard startingBoard (EnPassantMove (Field 2 2) (Field 3 3) (Field 3 7))
putStr $ showBoard $ updateBoard startingBoard (CastlingMove (Field 5 1) (Field 3 1) (Field 1 1) (Field 4 1))
:q

-}
