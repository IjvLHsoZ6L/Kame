import Control.Applicative ((<$>))
import Data.Array.Unboxed (UArray, array, range, bounds, indices, (!), (//))
import Data.Set (empty, singleton, notMember, insert)

main :: IO ()
main = do
    b0 <- toBoard <$> readFile "problem.txt"
    printBoard b0
    printBoardTrack $ head $ filter (isCompleted . fst) $ transform b0
    -- mapM_ printIntBoardTrack $ zip [1 .. ] $ transform b0

type Board = UArray Coord State

type State = Char

type Coord = (Int, Int)

type Route = [Coord]

type Track = [Route]

toBoard :: String -> Board
toBoard s = array bds [ ((i, j), lns !! i !! j) | (i, j) <- range bds ] where
    lns = lines s
    bds = ((0, 0), (h - 1, w - 1))
    h = length lns
    w = length (head lns)

fromBoard :: Board -> String
fromBoard b = unlines [ [ b ! (i , j) | j <- [jMin .. jMax] ] | i <- [iMin .. iMax] ] where
        ((iMin, jMin), (iMax, jMax)) = bounds b

printBoard :: Board -> IO ()
printBoard = putStrLn . fromBoard

printBoardTrack :: (Board, Track) -> IO ()
printBoardTrack (b, t) = do
    mapM_ print $ reverse $ map reverse t
    printBoard b

printIntBoardTrack :: (Int, (Board, Track)) -> IO ()
printIntBoardTrack (i, bt) = do
    print i
    printBoardTrack bt

isCompleted :: Board -> Bool
isCompleted b = and [ b ! c /= 'x' | c <- indices b ]

transform :: Board -> [(Board, Track)]
transform b0 = aux [] [] [(b0, [])] empty (singleton b0) where
    aux [] [] [] _ _
        = []
    aux [] [] bts bcset bset
        = aux [ (b, p, [], t) | (b, t) <- bts , p <- canCatch b ] [] [] bcset bset
    aux [] bcrtsNext bts bcset bset
        = aux bcrtsNext [] bts bcset bset
    aux bcrts @ ((b, c, r, t) : bcrts') bcrtsNext bts bcset bset
        | b `notMember` bset
            = (b, t') : aux bcrts bcrtsNext ((b, t') : bts) bcset (insert b bset)
        | (b, c) `notMember` bcset
            = aux bcrts' ([ (move b c c', c', c : r, t) | c' <- canMove b c ] ++ bcrtsNext)
                bts (insert (b, c) bcset) bset
        | otherwise
            = aux bcrts' bcrtsNext bts bcset bset
        where
            t' = (c : r) : t

canCatch :: Board -> [Coord]
canCatch b = [ c | c <- indices b , isPresent b c ]

canMove :: Board -> Coord -> [Coord]
canMove b c = [ c' | c' <- indices b , canJoin c c'
    , isBlank b c' , and [ isPresent b c'' | c'' <- segment c c' ] ]

move :: Board -> Coord -> Coord -> Board
move b c c' = b // ((c, '.') : (c', b ! c) : [ (c'', invert (b ! c'')) | c'' <- segment c c' ])

isPresent :: Board -> Coord -> Bool
isPresent b c = b ! c == 'o' || b ! c == 'x'

isBlank :: Board -> Coord -> Bool
isBlank b c = b ! c == '.'

invert :: State -> State
invert 'o' = 'x'
invert 'x' = 'o'
invert _   = error "invert"

canJoin :: Coord -> Coord -> Bool
canJoin (i, j) (i', j')
    | abs (i - i') <= 1 && abs (j - j') <= 1
        = False
    | otherwise
        = i == i' || j == j' || abs (i - i') == abs (j - j')

segment :: Coord -> Coord -> [Coord]
segment (i, j) (i', j')
    | i == i'
        = [ (i, j'') | j'' <- j''s ]
    | j == j'
        = [ (i'', j) | i'' <- i''s ]
    | i - i' == j - j'
        = zip i''s j''s
    | i - i' == j' - j
        = zip i''s (reverse j''s)
    | otherwise
        = error "segment"
    where
        i''s = [min i i' + 1 .. max i i' - 1]
        j''s = [min j j' + 1 .. max j j' - 1]
