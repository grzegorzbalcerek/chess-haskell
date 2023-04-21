module Chess.Game where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.List (find, sort, group)
import Chess.Color
import Chess.Figure
import Chess.Field
import Chess.Move
import Chess.FigureMoves
import Chess.Board

data Game = GameStart | OngoingGame Color Board [Game] Move

instance Show Game where
  show GameStart = "White to begin:\n" ++ showBoard startingBoard
  show (OngoingGame color board _ lastMove) =
    "Last move: " ++ (show $ other color) ++ " " ++
    (show $ Chess.Move.from lastMove) ++ " to " ++
    (show $ Chess.Move.to lastMove) ++ "\n" ++ (showBoard board)

gameColor GameStart = White
gameColor (OngoingGame color _ _ _) = color

gameHist GameStart = []
gameHist (OngoingGame _ _ hist _) = hist

gameBoard GameStart = startingBoard
gameBoard (OngoingGame _ board _ _) = board

-- | Verifies if the given field is empty.
isFieldEmpty game field = not (Map.member field (gameBoard game))

-- | Returns free fields onto which the figure may be moved.
freeDestinations :: Game -> [[Field]] -> [Field]
freeDestinations game fieldss =
  fieldss >>= (\fields -> takeWhile (isFieldEmpty game) fields)

-- | Returns fields occupied by the enemy figures
-- (including the case when that figure is the King)
-- onto which the figure may be moved.
captureDestinations :: Game -> [[Field]] -> [Field]
captureDestinations game fieldss =
    fieldss >>= filter hasEnemyFigure .
                take 1 .
                dropWhile (isFieldEmpty game)
  where hasEnemyFigure field =
          figureColor (fromJust (Map.lookup field (gameBoard game))) == other (gameColor game)
          
-- | Returns fields occupied by the enemy figures
-- (including the case when that figure is the King)
-- onto which the figure may be moved.
defendedDestinations :: Game -> [[Field]] -> [Field]
defendedDestinations game fieldss =
    fieldss >>= filter hasSameColorFigure .
                take 1 .
                dropWhile (isFieldEmpty game)
  where hasSameColorFigure field =
          figureColor (fromJust (Map.lookup field (gameBoard game))) == (gameColor game)

-- | Returns a new game, updated with a move.
updateGame :: Game -> Move -> Game
updateGame game move =
  OngoingGame (other (gameColor game))
              (updateBoard (gameBoard game) move)
              (game:(gameHist game))
              move

-- | Verifies if the enemy King is under check.
isOtherKingUnderCheck game = not $ all isKingOnBoard (nextGames game)
  where isKingOnBoard g = any (== Figure King (other $ gameColor game))
                              (Map.elems (gameBoard g))

-- | Verifies if the King of the player who is about to make a move is under check.
isKingUnderCheck :: Game -> Bool
isKingUnderCheck game =
  let newGame = OngoingGame (other $ gameColor game)
                            (gameBoard game)
                            (game:(gameHist game))
                            (RegularMove (Field 0 0) (Field 0 0))
  in isOtherKingUnderCheck newGame

-- | Verifies the conditions of when the castling move is permitted:
-- whether the King and the Rook are on their initial positions,
-- whether they were there from the begining of the game,
-- whether the fields between them are free and
-- whether the field to be passed by the King is checked or not.
-- If the given castling move is permitted, the method returns a one-element sequence.
-- Otherwise it returns an empty sequence.
castling game kingTo rookFrom rookTo otherCol =
  let color = gameColor game
      row = firstRow color
      hist = gameHist game
      board = gameBoard game
  in if Map.lookup (Field 5 row) board == Just (Figure King color) &&
        Map.lookup (Field rookFrom row) board == Just (Figure Rook color) &&
        Map.lookup (Field rookTo row) board == Nothing &&
        Map.lookup (Field kingTo row) board == Nothing &&
        Map.lookup (Field otherCol row) board == Nothing &&
        all (\g -> Map.lookup (Field 4 row) (gameBoard g) == Just (Figure King color)) hist &&
        all (\g -> Map.lookup (Field rookFrom row) (gameBoard g) == Just (Figure Rook color)) hist &&
        not (isOtherKingUnderCheck (updateGame game (RegularMove (Field 4 row) (Field rookTo row))))
     then [updateGame game (CastlingMove (Field 4 row) (Field kingTo row) (Field rookFrom row) (Field rookTo row))]
     else []

-- | Verifies if the en passant capture move is possible.
isEnPassantCapture GameStart _ _ = False
isEnPassantCapture (OngoingGame color board _ lastMove) from to =
  Map.lookup (Chess.Move.to lastMove) board == Just (Figure Pawn (other color)) &&
  Chess.Move.to lastMove == Field (col to) (row to) &&
  Chess.Move.from lastMove == Field (col to) (row to + 2 * (row to - row from))

-- | Returns next games after possible next moves moves (including those
-- moves after which the King is checked).
-- The code itereates over all figures that have the same color as
-- the color of the next move. The 'g' value contains sequences of game states
-- corresponding to the possible next moves of the given figure.
-- Figure moves depend on its kind. The Rook, the Knight, the Queen, the Bishop
-- and the King are treated in a similar way, except for the King, for which
-- the castling moves are included as well.
-- Potentially there are two possible castling moves.
-- Each of them is handled by a call to the 'castling' method.
-- The most complex case handled by the mthod is the case of the Pawn moves.
-- The Pawn may move forward onto a free field or forward and left or right onto
-- a field occupied by an enemy figure. In both cases, if the destination field
-- lies on the last row, the set of possible moves includes the possible
-- promotions to other figures. In addition to that, the Pawn may make the so
-- called en passant capture, which consists of moving the Pawn forward and left
-- or right onto a free field which has been passed-by by an enemy Pawn in the
-- previous move.
nextGames :: Game -> [Game]
nextGames game =
  [ nextGame |
    (from, figure) <- Map.toList (gameBoard game),
    figureColor figure == gameColor game,
    nextGame <- case figure of
      Figure fig _ | fig == Rook || fig == Bishop  ||
                     fig == Queen || fig == Knight || fig == King ->
                       (let fieldss = figureMoves figure from True
                        in map (\to -> updateGame game (RegularMove from to)) $
                           freeDestinations game fieldss ++ captureDestinations game fieldss) ++
                       (if fig == King
                        then castling game 3 1 4 2 ++ castling game 7 8 6 7
                        else [])
      Figure Pawn _ ->
        let regularAndPromotionMoves =
              concat [ freeDestinations game (figureMoves figure from False)
                     , captureDestinations game (figureMoves figure from True) ] >>=
                  \to -> if isLastRow to (gameColor game)
                         then map (\figureType -> updateGame game $ PromotionMove from to $
                                                  Figure figureType $ gameColor game)
                              [ Queen, Rook, Bishop, Knight ]
                         else [ updateGame game (RegularMove from to) ]
            enPassantMoves =
              map (\to -> updateGame game (EnPassantMove from to (Field (col to) (row from)))) .
              filter (isEnPassantCapture game from) $
              freeDestinations game (figureMoves figure from True)
        in regularAndPromotionMoves ++ enPassantMoves]

-- | Filters out the next games in which the king is under check.
validGames :: Game -> [Game]
validGames game = filter (not . isOtherKingUnderCheck) $ nextGames game

-- | Verifies if the game is over.
-- The following end game conditions are handled:
-- + after every possible move the King is under check,
-- + only the two Kings are left on the board,
-- + only the two Kings, one Bishop and one Knight are left on the board,
-- + only the two Kings and two Knights of the same color are left on the board,
-- + the same position occurred three times.
isGameFinished game =
  all isOtherKingUnderCheck (nextGames game) ||
  elem (Map.elems (gameBoard game))
       [ sort [ Figure King White, Figure King Black ],
         sort [ Figure King White, Figure King Black , Figure Bishop White ],
         sort [ Figure King White, Figure King Black , Figure Bishop Black ],
         sort [ Figure King White, Figure King Black , Figure Knight White ],
         sort [ Figure King White, Figure King Black , Figure Knight Black ],
         sort [ Figure King White, Figure King Black , Figure Knight White , Figure Knight White ],
         sort [ Figure King White, Figure King Black , Figure Knight Black , Figure Knight Black ]] ||
  (not . null . filter (\g -> length g >= 3) . group . sort . map gameBoard $ game : gameHist game)

-- | Returns 'Just' containing the color of the game winner or 'Nothing' if there is no winner.
winner game =
  if (isGameFinished game && isKingUnderCheck game)
  then Just . other . gameColor $ game
  else Nothing
                                                                                              
-- | Returns a new game state after moving a figure. If the given
-- move is not possible, it returns Nothing.
move :: Field -> Field -> Maybe Figure -> Game -> Maybe Game
move from to promotion game =
  find (const True) $ filter isMatching $ validGames game
  where isMatching (OngoingGame _ _ _ move) =
          case move of
            RegularMove f t -> f == from && t == to && isNothing promotion
            PromotionMove f t fig -> f == from && t == to && promotion == Just fig
            EnPassantMove f t _ -> f == from && t == to && isNothing promotion
            CastlingMove f t _ _ ->  f == from && t == to && isNothing promotion

{-

ghci
:load Chess/Game.hs
GameStart
isFieldEmpty GameStart (Field 2 2) -- False
isFieldEmpty GameStart (Field 2 3) -- True
freeDestinations GameStart $ figureMoves (Figure Rook White) (Field 3 4) False -- [d4,e4,f4,g4,h4,b4,a4,c5,c6,c3]
freeDestinations GameStart $ figureMoves (Figure Bishop White) (Field 3 4) False -- [d5,e6,b5,a6,d3,b3]
captureDestinations GameStart $ figureMoves (Figure Rook White) (Field 3 4) False -- [c7]
captureDestinations GameStart $ figureMoves (Figure Bishop White) (Field 3 4) False -- [f7]
isOtherKingUnderCheck GameStart -- False
isKingUnderCheck GameStart -- False
isGameFinished GameStart -- False
winner GameStart -- Nothing
length (nextGames GameStart) -- 20
length (validGames GameStart) -- 20
move (Field 1 2) (Field 1 5) Nothing GameStart -- Nothing
let Just g1 = move (Field 7 2) (Field 7 4) Nothing GameStart
let Just g2 = move (Field 5 7) (Field 5 6) Nothing g1
let Just g3 = move (Field 6 2) (Field 6 4) Nothing g2
let Just g4 = move (Field 4 8) (Field 8 4) Nothing g3
isOtherKingUnderCheck g4 -- False
isKingUnderCheck g4 -- True
isGameFinished g4 -- True
winner g4 -- Just Black
length (nextGames g4) -- 20
length (validGames g4) -- 0
:q

-}
