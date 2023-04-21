module Chess.FigureMoves where

import Chess.Color
import Chess.Figure
import Chess.Field

-- | Sequences of relative figure positions for rook moves.
rookMoves = [([1..],repeat 0),
             ([-1,-2..],repeat 0),
             (repeat 0,[1..]),
             (repeat 0, [-1,-2..])]

-- | Sequences of relative figure positions for bishop moves.
bishopMoves = [([1..],[1..]),
               ([-1,-2..],[1..]),
               ([1..],[-1,-2..]),
               ([-1,-2..],[-1,-2..])]

-- | Sequences of relative figure positions for queen moves.
queenMoves = rookMoves ++ bishopMoves

-- | Sequences of relative figure positions for knight moves.
knightMoves = [([1],[2]),
               ([2],[1]),
               ([-1],[2]),
               ([2],[-1]),
               ([-1],[-2]),
               ([-2],[-1]),
               ([1],[-2]),
               ([-2],[1])]

-- | Sequences of relative figure positions for king moves.
kingMoves = map (\(c,r) -> (take 1 c, take 1 r)) queenMoves

-- | Choose the sequences of relative figure positions
-- based on the figure position, type, color,
-- and whether the move is a capture move or not.
chooseFigureMoves :: Figure -> Field -> Bool -> [([Int],[Int])]
chooseFigureMoves (Figure Rook _)     f             _     = rookMoves
chooseFigureMoves (Figure Bishop _)   f             _     = bishopMoves
chooseFigureMoves (Figure King _)     f             _     = kingMoves
chooseFigureMoves (Figure Queen _)    f             _     = queenMoves
chooseFigureMoves (Figure Knight _)   f             _     = knightMoves
chooseFigureMoves (Figure Pawn White) f@(Field _ 2) False = [(repeat 0,[1,2])]
chooseFigureMoves (Figure Pawn White) f@(Field _ _) False = [([0],[1])]
chooseFigureMoves (Figure Pawn Black) f@(Field _ 7) False = [(repeat 0,[-1,-2])]
chooseFigureMoves (Figure Pawn Black) f@(Field _ _) False = [([0],[-1])]
chooseFigureMoves (Figure Pawn White) f@(Field _ _) True  = [([-1],[1]),([1],[1])]
chooseFigureMoves (Figure Pawn Black) f@(Field _ _) True  = [([-1],[-1]),([1],[-1])]

-- | Returns the field relative to the given field according to
-- a pair of relative coordinates.
relativeField (Field col row) (c,r) = Field (col+c) (row+r)

-- | Returns fields relative to the given field according to
-- the sequence of relative coordinates.
relativeFields field (cols,rows) =
  takeWhile isValid (map (relativeField field) (zip cols rows))

-- | Returns possible figure moves.
-- The figure is on the field 'field' and the 'capture' flag indicate whether
-- the move is a capture.
figureMoves :: Figure -> Field -> Bool -> [[Field]]
figureMoves figure field capture = map (relativeFields field) $ chooseFigureMoves figure field capture

{--

ghci
:load Chess/FigureMoves.hs
map (\(a,b) -> (take 4 a,take 4 b)) rookMoves -- [([1,2,3,4],[0,0,0,0]),([-1,-2,-3,-4],[0,0,0,0]),([0,0,0,0],[1,2,3,4]),([0,0,0,0],[-1,-2,-3,-4])]
map (\(a,b) -> (take 4 a,take 4 b)) bishopMoves -- [([1,2,3,4],[1,2,3,4]),([-1,-2,-3,-4],[1,2,3,4]),([1,2,3,4],[-1,-2,-3,-4]),([-1,-2,-3,-4],[-1,-2,-3,-4])]
map (\(a,b) -> (take 4 a,take 4 b)) queenMoves -- [([1,2,3,4],[0,0,0,0]),([-1,-2,-3,-4],[0,0,0,0]),([0,0,0,0],[1,2,3,4]),([0,0,0,0],[-1,-2,-3,-4]),([1,2,3,4],[1,2,3,4]),([-1,-2,-3,-4],[1,2,3,4]),([1,2,3,4],[-1,-2,-3,-4]),([-1,-2,-3,-4],[-1,-2,-3,-4])]
knightMoves -- [([1],[2]),([2],[1]),([-1],[2]),([2],[-1]),([-1],[-2]),([-2],[-1]),([1],[-2]),([-2],[1])]
kingMoves -- [([1],[0]),([-1],[0]),([0],[1]),([0],[-1]),([1],[1]),([-1],[1]),([1],[-1]),([-1],[-1])]
map (\(a,b) -> (take 4 a,take 4 b)) $ chooseFigureMoves (Figure Pawn White) (Field 1 2) False -- [([0,0,0,0],[1,2])]
map (\(a,b) -> (take 4 a,take 4 b)) $ chooseFigureMoves (Figure Pawn White) (Field 1 4) False -- [([0],[1])]
map (\(a,b) -> (take 4 a,take 4 b)) $ chooseFigureMoves (Figure Pawn Black) (Field 1 7) False -- [([0,0,0,0],[-1,-2])]
map (\(a,b) -> (take 4 a,take 4 b)) $ chooseFigureMoves (Figure Pawn Black) (Field 1 5) False -- [([0],[-1])]
map (\(a,b) -> (take 4 a,take 4 b)) $ chooseFigureMoves (Figure Pawn White) (Field 1 2) True -- [([-1],[1]),([1],[1])]
map (\(a,b) -> (take 4 a,take 4 b)) $ chooseFigureMoves (Figure Pawn Black) (Field 1 7) True -- [([-1],[-1]),([1],[-1])]
relativeField (Field 1 2) (1,1) -- b3
relativeField (Field 1 2) (0,2) -- a4
relativeFields (Field 2 2) ([0,0],[1,2]) -- [b3,b4]
figureMoves (Figure Rook White) (Field 3 4) False -- [[d4,e4,f4,g4,h4],[b4,a4],[c5,c6,c7,c8],[c3,c2,c1]]
figureMoves (Figure Pawn White) (Field 2 2) False -- [[b3,b4]]
figureMoves (Figure Pawn White) (Field 2 2) True -- [[a3],[c3]]
figureMoves (Figure Pawn White) (Field 1 2) True -- [[],[b3]]
:q

-}
