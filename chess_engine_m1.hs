module ChessEngine2 where 

type Board = [[Square]]
type Square = Maybe Piece

data Piece = Piece PColor PType deriving (Show)
data PColor = Black | White deriving (Show)
data PType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show)

readSquareChar :: Square -> Char
readSquareChar Nothing = " "
readSquareChar (Just piece) = pieceToChar piece


pieceToChar :: Piece -> Char
pieceToChar (Piece Black King) = 'K'
pieceToChar (Piece Black Queen) = 'Q'
pieceToChar (Piece Black Bishop) = 'B'
pieceToChar (Piece Black Knight) = 'G'
pieceToChar (Piece Black Rook) = 'R'
pieceToChar (Piece Black Pawn) = 'P'
pieceToChar (Piece White King) = 'k'
pieceToChar (Piece White Queen) = 'q'
pieceToChar (Piece White Bishop) = 'b'
pieceToChar (Piece White Knight) = 'g'
pieceToChar (Piece White Rook) = 'r'
pieceToChar (Piece White Pawn) = 'p'

charToPiece :: Char -> Piece
charToPiece 'K' = (Piece Black King)  
charToPiece 'Q' = (Piece Black Queen) 
charToPiece 'B' = (Piece Black Bishop)
charToPiece 'G' = (Piece Black Knight)
charToPiece 'R' = (Piece Black Rook)
charToPiece 'P' = (Piece Black Pawn)
charToPiece 'k' = (Piece White King)
charToPiece 'q' = (Piece White Queen) 
charToPiece 'b' = (Piece White Bishop)
charToPiece 'g' = (Piece White Knight)
charToPiece 'r' = (Piece White Rook) 
charToPiece 'p' = (Piece White Pawn)

