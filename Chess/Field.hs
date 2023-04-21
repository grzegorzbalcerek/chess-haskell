module Chess.Field where

import Data.Char
import Chess.Color

-- | The game of chess is played on a board with 64 fields.
-- The board has the shape of a square with eight rows — from 1 to 8
-- — and eight columns — from `a` to `h`.
-- The 'Field' represents a board field.
-- It has members representing the column and the row —
-- the field coordinates on the chess board.
-- Valid fields have coordinates in the range between 1 and 8.
data Field = Field { col :: Int, row :: Int }
  deriving (Eq, Ord)

-- | Shows field coordinates as a pair of characters:
-- a letter representing the column and a number representing the row.
instance Show Field where
  show f = chr (ord 'a' + (col f) - 1) : show (row f)

-- | Returns a new field with coordinates moved
-- by the given number of rows and columns relative to the original field.
relative (Field col row) c r = Field (col+c) (row+r)

-- | Returns a boolean value indicating
-- whether the given field belongs to the last row from
-- the point of view of a player.
isLastRow :: Field -> Color -> Bool
isLastRow (Field _ row) color = row == firstRow (other color)

-- | Returns a boolean value indicating
-- whether the field has valid coordinates, that is
-- whether it belongs to the board.
isValid :: Field -> Bool
isValid (Field col row) = col >= 1 && col <= 8 && row >= 1 && row <= 8

{-

ghci
:load Chess/Field.hs
Field 1 4 -- a4
col (Field 1 4) -- 1
row (Field 1 4) -- 4
relative (Field 1 4) 1 1 -- b5
isLastRow (Field 2 1) Black -- True
isValid (Field 2 1) -- True
:q

-}
