import Data.Array.Unboxed
import Data.Set (empty, member, insert)

main :: IO ()
main = do
    s <- readFile "problem.txt"
    let bStart = toBoard s
    printBoard bStart
    mapM_ printBoardProcess [ (b, pr) | (b, pr) <- tranforms bStart , isClear b ]
    where

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
    print (reverse (map reverse pr))
    printBoard b

tranforms :: Board -> [(Board, Process)]
tranforms bStart = loop empty [(bStart, [])] where
    loop _ [] = []
    loop found ((b, pr) : rest)
        | b `member` found
            = loop found rest
        | otherwise
            = (b, pr)
            : loop (b `insert` found) (rest
                ++ [ (b', path : pr) | (b', path) <- tranformsByPath b ])

tranformsByPath :: Board -> [(Board, Path)]
tranformsByPath bStart = loop empty [(bStart, p, [p]) | p <- positions , exists bStart p ] where
    loop _ [] = []
    loop found ((b, p, path) : rest)
        | (b, p) `member` found
            = loop found rest
        | otherwise
            = (b, path)
            : loop ((b, p) `insert` found) (rest
                ++ [ (move b p q, q, q : path) | q <- positions , canMove b p q ])

isClear :: Board -> Bool
isClear b = and [ b ! p /= 'x' | p <- positions ]

canMove :: Board -> Position -> Position -> Bool
canMove b p q = isBlank b q
    && canJoin p q
    && and [ exists b r | r <- segment p q ]

move :: Board -> Position -> Position -> Board
move b p q = b
    // [ (p, '.') ]
    // [ (r, invert (b ! r)) | r <- segment p q ]
    // [ (q, b ! p) ]

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
    | i0 == i1
        = True
    | j0 == j1
        = True
    | abs (i1 - i0) == abs (j1 - j0)
        = True
    | otherwise
        = False

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
