import Control.Applicative ((<$>))
import Data.Array.Unboxed (UArray, array, range, (!), (//))
import Data.Set (Set, empty, singleton, notMember, insert)
import Data.Sequence (Seq, ViewL(EmptyL, (:<)), viewl, fromList, (|>), (><))
import qualified Data.Sequence as Q (empty, singleton)

main :: IO ()
main = do
    b0 <- toBoard <$> readFile "problem.txt"
    printBoard b0
    printBoardProcess $ head $ filter (isClear . fst) $ transform b0
    -- mapM_ printBoardProcess $ transform b0

type Board = UArray Position State

type State = Char

type Position = (Int, Int)

type Path = [Position]

type Process = [Path]

height :: Int
height = 4

width :: Int
width = 4

positionBound :: (Position, Position)
positionBound = ((0, 0), (height - 1, width - 1))

positions :: [Position]
positions = range positionBound

toBoard :: String -> Board
toBoard s = array positionBound [ ((i, j), lines s !! i !! j) | (i, j) <- positions ]

fromBoard :: Board -> String
fromBoard b = unlines [ [ b ! (i, j) | j <- [0 .. width - 1] ] | i <- [0 .. height - 1] ]

printBoard :: Board -> IO ()
printBoard = putStrLn . fromBoard

printBoardProcess :: (Board, Process) -> IO ()
printBoardProcess (b, pr) = do
    mapM_ print (reverse (map reverse pr))
    printBoard b

isClear :: Board -> Bool
isClear b = and [ b ! p /= 'x' | p <- positions ]

transform :: Board -> [(Board, Process)]
transform b0 = loop Q.empty (Q.singleton (b0, [])) empty (singleton b0) where
    loop :: Seq (Board, Position, Path, Process) -> Seq (Board, Process)
            -> Set (Board, Position) -> Set Board -> [(Board, Process)]
    loop bppps bprs bpset bset = case viewl bppps of
        EmptyL -> case viewl bprs of
            EmptyL
                -> []
            (b, pr) :< bprs'
                -> loop (bppps >< fromList [ (b, p, [], pr) | p <- positions , exists b p ]) bprs' bpset bset
        (b, p, ps, pr) :< bppps'
            | b `notMember` bset
                -> (b, (p : ps) : pr)
                : loop bppps (bprs |> (b, (p : ps) : pr)) bpset (b `insert` bset)
            | (b, p) `notMember` bpset
                -> loop (bppps' >< fromList [ (move b p q, q, p : ps, pr) | q <- positions , canMove b p q ])
                    bprs ((b, p) `insert` bpset) bset
            | otherwise
                -> loop bppps' bprs bpset bset

canMove :: Board -> Position -> Position -> Bool
canMove b p q = isBlank b q && canJoin p q && and [ exists b r | r <- segment p q ]

move :: Board -> Position -> Position -> Board
move b p q = b // ((p, '.') : (q, b ! p) : [ (r, invert (b ! r)) | r <- segment p q ])

exists :: Board -> Position -> Bool
exists b p = b ! p == 'o' || b ! p == 'x'

isBlank :: Board -> Position -> Bool
isBlank b p = b ! p == '.'

invert :: State -> State
invert 'o' = 'x'
invert 'x' = 'o'
invert _   = error "invert"

canJoin :: Position -> Position -> Bool
canJoin (i0, j0) (i1, j1)
    | abs (i1 - i0) <= 1 && abs (j1 - j0) <= 1
        = False
    | otherwise
        = i0 == i1 || j0 == j1 || abs (i1 - i0) == abs (j1 - j0)

segment :: Position -> Position -> [Position]
segment (i0, j0) (i1, j1)
    | i0 == i1
        = [ (i0, j) | j <- js ]
    | j0 == j1
        = [ (i, j0) | i <- is ]
    | i1 - i0 == j1 - j0
        = zip is js
    | i1 - i0 == j0 - j1
        = zip is (reverse js)
    | otherwise
        = error "segment"
    where
        is = [min i0 i1 + 1 .. max i0 i1 - 1]
        js = [min j0 j1 + 1 .. max j0 j1 - 1]
