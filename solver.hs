import Control.Applicative
import Data.Array.Unboxed
import qualified Data.Set as S
import qualified System.Environment

main :: IO ()
main = do
    b0 <- fmap toBoard . (=<<) readFile . fmap head $ System.Environment.getArgs
    printBoard b0
    printBoardTrack . head . filter (hasNoReverse . fst) . transforms $ b0

type Board = UArray Coord State

type State = Char

type Coord = (Int, Int)

type Track = [[Coord]]

toBoard :: String -> Board
toBoard s = array bds [ ((i, j), lns !! i !! j) | (i, j) <- range bds ] where
    lns = lines s
    bds = ((0, 0), (h - 1, w - 1))
    h = length lns
    w = length (head lns)

fromBoard :: Board -> String
fromBoard b = unlines [ [ b ! (i, j) | j <- [jMin .. jMax] ] | i <- [iMin .. iMax] ] where
    ((iMin, jMin), (iMax, jMax)) = bounds b

printBoard :: Board -> IO ()
printBoard = putStrLn . fromBoard

printBoardTrack :: (Board, Track) -> IO ()
printBoardTrack (b, t) = mapM_ print (reverse . map reverse $ t) >> printBoard b

printIntBoardTrack :: (Int, (Board, Track)) -> IO ()
printIntBoardTrack (i, bt) = print i >> printBoardTrack bt

hasNoReverse :: Board -> Bool
hasNoReverse b = and [ b ! c /= 'x' | c <- indices b ]

transforms :: Board -> [(Board, Track)]
transforms b0 = aux [] [] [(b0, [])] (S.singleton b0) S.empty where
    aux [] [] [] _ _
        = []
    aux [] [] bts bset bcset
        = aux [] (map picks bts) [] bset bcset
    aux [] bcptss bts bset bcset
        = aux (concat . reverse $ bcptss) [] bts bset bcset
    aux bcpts @ ((b, c, r, t) : bcpts') bcptss bts bset bcset
        | S.notMember b bset
            = (b, t') : aux bcpts bcptss ((b, t') : bts) (S.insert b bset) bcset
        | S.notMember (b, c) bcset
            = aux bcpts' (moves (b, c, r, t) : bcptss) bts bset (S.insert (b, c) bcset)
        | otherwise
            = aux bcpts' bcptss bts bset bcset
        where t' = (c : r) : t
    picks (b, t) = [ (b, c, [], t) | c <- indices b , b `hasPieceOn` c ]
    moves (b, c, r, t) = [ (move b c c', c', c : r, t) | c' <- indices b , b `admitsMove` (c, c') ]

hasPieceOn :: Board -> Coord -> Bool
hasPieceOn b c = b ! c /= '.'

admitsMove :: Board -> (Coord, Coord) -> Bool
admitsMove b (c0, c1)
    = b `hasPieceOn` c0
    && not (b `hasPieceOn` c1)
    && hasSegment (c0, c1)
    && and [ b `hasPieceOn` c | c <- segment (c0, c1) ]

move :: Board -> Coord -> Coord -> Board
move b c0 c1 = b // ((c0, '.') : (c1, b ! c0) : [ (c, invert (b ! c)) | c <- segment (c0, c1) ])

invert :: State -> State
invert 'o' = 'x'
invert 'x' = 'o'
invert _   = error "invert"

hasSegment :: (Coord, Coord) -> Bool
hasSegment ((i0, j0), (i1, j1)) = (di > 1 || dj > 1) && (di == 0 || dj == 0 || di == dj) where
    di = abs (i1 - i0)
    dj = abs (j1 - j0)

segment :: (Coord, Coord) -> [Coord]
segment ((i0, j0), (i1, j1))
    | i0 == i1
        = zip (repeat i0) js
    | j0 == j1
        = zip is (repeat j0)
    | i1 - i0 == j1 - j0
        = zip is js
    | i1 - i0 == j0 - j1
        = zip is (reverse js)
    | otherwise
        = error "segment"
    where
        is = [min i0 i1 + 1 .. max i0 i1 - 1]
        js = [min j0 j1 + 1 .. max j0 j1 - 1]
