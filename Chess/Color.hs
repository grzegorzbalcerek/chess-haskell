module Chess.Color where

-- | The 'Color' class represents one of the two colors ('Black' or 'White')
-- used in the game of Chess.
data Color = White | Black
  deriving (Eq,Show,Ord)

-- | The 'other' method returns the opposite color.
other :: Color -> Color
other White = Black
other Black = White

-- | The 'firstRow' method returns the coordinate of the first row
-- from the point of view of a player who plays the given color.
firstRow :: Color -> Int
firstRow White = 1
firstRow Black = 8

{-

ghci
:load Chess/Color.hs
other White -- Black
other Black -- White
firstRow White -- 1
firstRow Black -- 8
:q

-}
