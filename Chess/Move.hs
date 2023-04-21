module Chess.Move where

import Chess.Field
import Chess.Figure

data Move = RegularMove   { from :: Field, to :: Field }
          | PromotionMove { from :: Field, to :: Field, figure :: Figure }
          | EnPassantMove { from :: Field, to :: Field, captured :: Field }
          | CastlingMove  { from :: Field, to :: Field, rookFrom :: Field, rookTo :: Field }
  deriving Show

{-

ghci
:load Chess/Move.hs
:q

-}
