module Chess.Rank where

import qualified Data.Map as Map
import Data.List (sortBy)
import Chess.Color
import Chess.Figure
import Chess.Field
import Chess.Move
import Chess.FigureMoves
import Chess.Game

-- | Returns the rank of a figure of the given type.
figureRank :: Figure -> Int
figureRank = figureTypeRank . figureType
  where figureTypeRank Queen = 900
        figureTypeRank Rook = 450
        figureTypeRank Knight = 300
        figureTypeRank Bishop = 300
        figureTypeRank Pawn = 100
        figureTypeRank _ = 0

-- |  Returns the rank of the given field.
fieldRank :: Field -> Int
fieldRank (Field col row) = 2*colRowRank(col) * colRowRank(row)
  where colRowRank cr = if cr>=5 then 9-cr else cr

-- | Returns the figure rank based on the figures it is defending.
figureDefendingOtherFiguresRank :: Game -> Field -> Figure -> Int
figureDefendingOtherFiguresRank game field figure =
  (length $ defendedDestinations game (figureMoves figure field True)) `div` 2

-- | Returns a rank value related to whether the King is under check or not.
checkRank :: Game -> Color -> Int
checkRank game color =
  if (gameColor game == other color) && isKingUnderCheck game then 50 else 0

-- | Calculates the position rank taking one color into account.
colorRank :: Game -> Color -> Int
colorRank game color =
  let ranks = [ r1+r2+r3 |
                (field,figure) <- Map.toList $ gameBoard game,
                figureColor figure == color,
                let r1 = figureRank figure,
                let r2 = fieldRank field,
                let r3 = figureDefendingOtherFiguresRank game field figure ]
  in sum ranks + checkRank game color

-- | Calculates the position rank from the point of view of a player.
rank :: Game -> Color -> Int
rank game color = colorRank game color - colorRank game (other color)

{-

ghci
:load Chess/Rank.hs
figureRank (Figure Queen White) -- 900
figureRank (Figure Knight Black) -- 300
fieldRank (Field 1 1) -- 2
fieldRank (Field 2 5) -- 16
fieldRank (Field 4 4) -- 32
let Just g1 = move (Field 1 2) (Field 1 3) Nothing GameStart
let Just g2 = move (Field 1 7) (Field 1 6) Nothing g1
figureDefendingOtherFiguresRank g2 (Field 2 1) (Figure Knight White) -- 1
let Just g1 = move (Field 7 2) (Field 7 4) Nothing GameStart
let Just g2 = move (Field 5 7) (Field 5 6) Nothing g1
let Just g3 = move (Field 6 2) (Field 6 4) Nothing g2
let Just g4 = move (Field 4 8) (Field 8 4) Nothing g3
checkRank GameStart White -- 0
checkRank g4 White -- 0
checkRank g4 Black -- 50
colorRank GameStart White -- 3928
colorRank g1 White -- 3928
colorRank g2 White -- 3935
colorRank g3 White -- 3940
colorRank g4 White -- 3947
rank GameStart White -- 8
rank g1 White -- 0
rank g2 White -- 7
rank g3 White -- 5
rank g4 White -- -32
rank GameStart Black -- -8
:q

-}
