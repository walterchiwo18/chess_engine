module ChessEngine where

import Data.Char
import Control.Error.Util (note)

type Board = [[Square]]
type Square = Maybe Piece

data Piece = Piece PColor PType deriving (Show)
data PColor = Black | White deriving (Show)
data PType = Queen | King | Knight | Rook | Bishop | Pawn deriving (Show)

readBoard :: String -> Maybe Board 
readBoard = mapM (mapM readSquare) . lines 
           
          

showBoard :: Maybe Board -> String
showBoard = unlines . (map . map) showSquare  

initStartPositions = unlines ["rgbqkbgr",
                              "pppppppp",
                              "        ",
                              "        ",
                              "PPPPPPPP",
                              "RGBQKBGR"]

readSquare :: Char -> Either String Square
readSquare '.' = Just Nothing
readSquare c = fmap Just (readPiece c)

showSquare :: Square -> Char
showSquare = maybe ' ' showPiece

showPiece :: Piece -> Char
showPiece (Piece Black King) = 'k'
showPiece (Piece Black Queen) = 'q'
showPiece (Piece Black Knight) = 'g'
showPiece (Piece Black Bishop) = 'b'
showPiece (Piece Black Rook) = 'r'
showPiece (Piece Black Pawn) = 'p'
showPiece (Piece White King) = 'K'
showPiece (Piece White Queen) = 'Q'
showPiece (Piece White Knight) = 'G'
showPiece (Piece White Bishop) = 'B'
showPiece (Piece White Pawn) = 'p'
showPiece (Piece White Rook) = 'R'

typeList :: [(Char, PType)]
typeList = [('k', King)
           ,('q', Queen)
           ,('b', Bishop)
           ,('g', Knight)
           ,('r', Rook)
           ,('p', Pawn)]

readPiece :: Char -> Maybe Piece
readPiece c = fmap partial_p ptype 
            where color = if (isUpper c) then White else Black
                  ptype = (lookup (toLower c) typeList)
                  partial_p = Piece color
