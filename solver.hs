import Control.Applicative ((<$>))
import Data.Array.Unboxed (UArray, array, range, bounds, indices, (!), (//))
import Data.Set (empty, singleton, notMember, insert)

main :: IO ()
main = do
    b0 <- toBoard <$> readFile "problem.txt"
    printBoard b0
    -- printBoardTrack $ head $ filter (isCompleted . fst) $ transforms b0
    mapM_ printIntBoardTrack $ zip [1 .. ] $ transforms b0

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

transforms :: Board -> [(Board, Track)]
transforms b0 = aux emptyQ (singletonQ (b0, [])) empty (singleton b0) where
    aux bcrts bts bcset bset
        | nullQ bcrts && nullQ bts
            = []
        | nullQ bcrts
            = aux (concatMapQ catches bts) emptyQ bcset bset
        | b `notMember` bset
            = (b, t') : aux bcrts (bts `enQ` (b, t')) bcset (insert b bset)
        | (b, c) `notMember` bcset
            = aux (bcrts' `appendQ` moves (b, c, r, t))
                bts (insert (b, c) bcset) bset
        | otherwise
            = aux bcrts' bts bcset bset
        where
            (b, c, r, t) = headQ bcrts
            bcrts' = tailQ bcrts
            t' = (c : r) : t
    catches (b, t) = [ (b, c, [], t) | c <- canCatch b ]
    moves (b, c, r, t) = [ (move b c c', c', c : r, t) | c' <- canMove b c ]

canCatch :: Board -> [Coord]
canCatch b = [ c | c <- indices b , exists b c ]

canMove :: Board -> Coord -> [Coord]
canMove b c = [ c' | c' <- indices b , canJoin c c'
    , isBlank b c' , and [ exists b c'' | c'' <- segment c c' ] ]

move :: Board -> Coord -> Coord -> Board
move b c c' = b // ((c, '.') : (c', b ! c) : [ (c'', invert (b ! c'')) | c'' <- segment c c' ])

exists :: Board -> Coord -> Bool
exists b c = b ! c == 'o' || b ! c == 'x'

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

type Queue a = ([a], [[a]])

toQ :: [a] -> Queue a
toQ xs = (xs, [])

fromQ :: Queue a -> [a]
fromQ (xs, xss) = xs ++ concat (reverse xss)

emptyQ :: Queue a
emptyQ = toQ []

singletonQ :: a -> Queue a
singletonQ x = toQ [x]

nullQ :: Queue a -> Bool
nullQ (xs, _) = null xs

headQ :: Queue a -> a
headQ (xs, _) = head xs

tailQ :: Queue a -> Queue a
tailQ (xs, ys) = validQ (tail xs, ys)

validQ :: Queue a -> Queue a
validQ ([], xss) = (concat (reverse xss), [])
validQ q         = q

enQ :: Queue a -> a -> Queue a
q `enQ` x = q `appendQ` [x]

appendQ :: Queue a -> [a] -> Queue a
(xs, xss) `appendQ` ys = validQ (xs, ys : xss)

concatMapQ :: (a -> [b]) -> Queue a -> Queue b
concatMapQ f = toQ . concatMap f . fromQ
