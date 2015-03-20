module FlipIt where

import Data.Array.Unboxed (UArray, array, bounds, indices, inRange, range, (!), (//))
import Data.Maybe (catMaybes)
import qualified Data.Set as S (empty, insert, notMember, singleton)

type Board = UArray Coord State

type State = Char -- '.' | 'o' | 'x'

type Coord = (Int, Int)

type Route = [Coord]

type Track = [Route]

parse :: String -> Board
parse s = array bds kvs where
    bds = ((0, 0), (h - 1, w - 1))
    kvs = [ ((i, j), lns !! i !! j) | (i, j) <- range bds ]
    lns = lines s
    h = length lns
    w = minimum $ map length lns

printBoard :: Board -> IO ()
printBoard b = putStrLn . unlines
    $ [ [ b ! (i, j) | j <- [jMin .. jMax] ] | i <- [iMin .. iMax] ]
    where ((iMin, jMin), (iMax, jMax)) = bounds b

printBoardTrack :: (Board, Track) -> IO ()
printBoardTrack (b, t) = do
    print . reverse . map reverse $ t
    printBoard b

isClear :: Board -> Bool
isClear b = and [ b ! c /= 'x' | c <- indices b ]

transforms :: Board -> [(Board, Track)]
transforms b0 = aux [] [] [(b0, [])] (S.singleton b0) S.empty where
    aux [] [] [] _ _
        = []
    aux [] [] bts bset bcset
        = aux [] (map picks' bts) [] bset bcset
    aux [] bcrtss bts bset bcset
        = aux (concat . reverse $ bcrtss) [] bts bset bcset
    aux bcrts @ ((b, c, r, t) : bcrts') bcrtss bts bset bcset
        | S.notMember b bset
            = (b, t') : aux bcrts bcrtss ((b, t') : bts) (S.insert b bset) bcset
        | S.notMember (b, c) bcset
            = aux bcrts' (moves' (b, c, r, t) : bcrtss) bts bset (S.insert (b, c) bcset)
        | otherwise
            = aux bcrts' bcrtss bts bset bcset
        where t' = (c : r) : t
    picks' (b, t) = [ (b, c, [], t) | c <- pieces b ]
    moves' (b, c, r, t) = [ (b', c', c : r, t) | (b', c') <- moves (b, c) ]

pieces :: Board -> [Coord]
pieces b = [ c | c <- indices b , b ! c /= '.' ]

moves :: (Board, Coord) -> [(Board, Coord)]
moves (b, c @ (ci, cj)) = catMaybes [ aux d 1 | d <- dirs ] where
    aux d @ (di, dj) n
        | not $ inRange (bounds b) c'
            = Nothing
        | n == 1 && b ! c' == '.'
            = Nothing
        | b ! c' == '.'
            = Just (b', c')
        | otherwise
            = aux d (n + 1)
        where
            b' = b // ([ (c, '.') , (c', b ! c) ] ++ [ (c'', invert $ b ! c'') | c'' <- c''s ])
            c' = (ci + n * di, cj + n * dj)
            c''s = [ (ci + k * di, cj + k * dj) | k <- [1 .. n - 1] ]

dirs :: [Coord]
dirs = [ (i, j) | i <- [-1 .. 1] , j <- [-1 .. 1] , not $ i == 0 && j == 0 ]

invert :: State -> State
invert 'o' = 'x'
invert 'x' = 'o'
invert _   = error "invert"
