import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Exception.Base

data Column = A|B|C|D|E|F|G|H deriving (Show, Eq, Enum, Bounded, Ord)

data Row = One|Two|Three|Four|Five|Six|Seven|Eight deriving (Show, Eq, Enum, Bounded, Ord)

data Coordinate = Coordinate {column :: Column, row :: Row} deriving (Eq, Ord)
instance Show Coordinate where
    show (Coordinate column row) = show column ++ " " ++ show row

data Team = White|Black deriving (Show, Eq)

data ChessPiece = Pawn|Knight|Bishop|Rook|Queen|King deriving (Show)

data GamePiece = GamePiece {team :: Team, piece :: ChessPiece}
instance Show GamePiece where
    show (GamePiece team piece) = show team ++ " " ++ show piece

newtype ChessBoard = ChessBoard { getChessBoard :: M.Map Coordinate GamePiece }

data ChessMove = ChessMove { source :: Coordinate, dest :: Coordinate }
instance Show ChessMove where
    show (ChessMove source dest) = show source ++ " -> " ++ show dest

data ChessGame = ChessGame { gameTurn :: Team, board :: ChessBoard}

class Display a where
    display :: a -> String

instance Display ChessGame where
    display game = display $ board game

instance Display ChessBoard where
    display board =
        let rowBorder = " " ++ take 23 (repeat '-') ++ "\n" ;
            createSquare board row column = maybe
                                            "  "
                                            display $ M.lookup
                                                        (Coordinate column row)
                                                        (getChessBoard board) 
            intercalateWithEnds xs xss = xs ++ intercalate xs xss ++ xs ;
            displayRow board row = intercalateWithEnds
                                   "|"
                                   (map
                                    (createSquare board row)
                                    [minBound :: Column .. maxBound :: Column])
                                   ++
                                   "\n" in
        intercalateWithEnds
        rowBorder
        (map
         (displayRow board)
         (reverse [minBound :: Row .. maxBound :: Row]))

instance Display GamePiece where
    display gamePiece = (display $ team gamePiece) ++ ( display $ piece gamePiece)

instance Display ChessPiece where
    display Pawn = "p"
    display Knight = "n"
    display Bishop = "b"
    display Rook = "r"
    display Queen = "q"
    display King = "k"

instance Display Team where
    display White = "W"
    display Black = "B"

isValid :: ChessMove -> ChessGame -> Bool
isValid move game = let theBoard = getChessBoard $ board game
                        turn = gameTurn game in
                    case M.lookup (source move) theBoard of 
                      Just friendly -> turn == team friendly &&
                                       case M.lookup (dest move) theBoard of
                                         Just enemy -> turn /= team enemy &&
                                                       isLegalAttack friendly (source move) (dest move)
                                         Nothing        -> isLegalMove friendly (source move) (dest move)
                      Nothing           -> False
safeSucc :: (Enum a, Bounded a, Eq a) => a -> Maybe a 
safeSucc x | x == maxBound = Nothing 
           | otherwise     = Just (succ x)
safePred :: (Enum a, Bounded a, Eq a) => a -> Maybe a 
safePred x | x == minBound = Nothing 
           | otherwise = Just (pred x) 

legalMoves :: GamePiece -> Coordinate -> [ChessMove]
legalMoves (GamePiece White Pawn) source =
    let sourceColumn = column source
        sourceRow    = row source
        destCoords = map (\row -> (Coordinate sourceColumn (succ row)))
                     (catMaybes (safeSucc sourceRow : safePred sourceRow : [])) in
    map (\dest -> ChessMove source dest) destCoords

isLegalMove :: GamePiece -> Coordinate -> Coordinate -> Bool
isLegalMove (GamePiece team Pawn) source dest =
    let sourceRow = row source
        destRow = row dest in
    case team of
      White ->
          succ sourceRow == destRow ||
          (sourceRow == Two &&
           succ (succ sourceRow) == destRow)
      Black ->
          pred sourceRow == destRow ||
          (sourceRow == Seven &&
           pred (pred sourceRow) == destRow)
--isLegalMove (GamePiece _ Knight) source dest =
    

isLegalAttack :: GamePiece -> Coordinate -> Coordinate -> Bool
isLegalAttack (GamePiece team Pawn) source dest = 
    let sourceColumn = column source
        destColumn = column dest
        sourceRow = row source
        destRow = row dest in
    (succ sourceColumn == destColumn ||
     pred sourceColumn == destColumn) &&
    case team of
      White ->
          succ sourceRow == destRow
      Black ->
          pred sourceRow == destRow
isLegalAttack (GamePiece _ Knight) source dest = False
isLegalAttack (GamePiece _ Bishop) source dest = False
isLegalAttack (GamePiece _ Rook) source dest = False
isLegalAttack (GamePiece _ Queen) source dest = False
isLegalAttack (GamePiece _ King) source dest = False

aOne     = Coordinate A One
aTwo     = Coordinate A Two
whiteKnight = GamePiece White Knight
whitePiece = GamePiece White
blackPiece = GamePiece Black 

whitePieces = [(Coordinate A One, whitePiece Rook),
               (Coordinate B One, whitePiece Knight),
               (Coordinate C One, whitePiece Bishop),
               (Coordinate D One, whitePiece Queen),
               (Coordinate E One, whitePiece King),
               (Coordinate F One, whitePiece Bishop),
               (Coordinate G One, whitePiece Knight),
               (Coordinate H One, whitePiece Rook),
               (Coordinate A Two, whitePiece Pawn),
               (Coordinate B Two, whitePiece Pawn),
               (Coordinate C Two, whitePiece Pawn),
               (Coordinate D Two, whitePiece Pawn),
               (Coordinate E Two, whitePiece Pawn),
               (Coordinate F Two, whitePiece Pawn),
               (Coordinate G Two, whitePiece Pawn),
               (Coordinate H Two, whitePiece Pawn)]
blackPieces = [(Coordinate A Eight, blackPiece Rook),
               (Coordinate B Eight, blackPiece Knight),
               (Coordinate C Eight, blackPiece Bishop),
               (Coordinate D Eight, blackPiece Queen),
               (Coordinate E Eight, blackPiece King),
               (Coordinate F Eight, blackPiece Bishop),
               (Coordinate G Eight, blackPiece Knight),
               (Coordinate H Eight, blackPiece Rook),
               (Coordinate A Seven, blackPiece Pawn),
               (Coordinate B Seven, blackPiece Pawn),
               (Coordinate C Seven, blackPiece Pawn),
               (Coordinate D Three, blackPiece Pawn),
               (Coordinate E Seven, blackPiece Pawn),
               (Coordinate F Seven, blackPiece Pawn),
               (Coordinate G Seven, blackPiece Pawn),
               (Coordinate H Seven, blackPiece Pawn)]

game = ChessGame White (ChessBoard $ M.fromList (whitePieces ++ blackPieces))

goodMove = isValid (ChessMove (Coordinate E Two) (Coordinate D Three)) game
badMove  = isValid (ChessMove (Coordinate D Two) (Coordinate D Three)) game



