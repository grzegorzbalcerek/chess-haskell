module Chess.Figure where

import Chess.Color

-- | Represents chess figure types.
data FigureType = King | Queen | Rook | Bishop | Knight | Pawn
  deriving (Show,Eq,Ord)

-- | Represents a figure, which has a type and a color.
data Figure = Figure { figureType :: FigureType, figureColor :: Color }
  deriving (Eq,Ord)

instance Show Figure where
  -- | Returns a one-character string representing the figure.
  show (Figure King   White) = "k"
  show (Figure Queen  White) = "q"
  show (Figure Rook   White) = "r"
  show (Figure Bishop White) = "b"
  show (Figure Knight White) = "n"
  show (Figure Pawn   White) = "p"
  show (Figure King   Black) = "K"
  show (Figure Queen  Black) = "Q"
  show (Figure Rook   Black) = "R"
  show (Figure Bishop Black) = "B"
  show (Figure Knight Black) = "N"
  show (Figure Pawn   Black) = "P"

{-

ghci
:load Chess/Figure.hs
Figure King White -- k
Figure King Black -- K
Figure Queen Black -- Q
Figure Rook Black -- R
:q

-}
