import Data.Array.Unboxed
import Data.Maybe
import qualified Data.Set as S

main :: IO ()
main = sequence_
    [ do
        print p
        printBoard b0
    | b0 <- map (array bds) $ allMap (range bds) ['.', 'x']
    , let bps = filter (isCleared . fst) $ takeWhile ((<= 2) . length . snd) $ transforms b0
    , not $ null bps
    , let (_, p) = head bps
    ]
    where bds = ((1, 1), (4, 4))

type Board = UArray Coord State

type Coord = (Int, Int)

type State = Char

printBoard :: Board -> IO ()
printBoard b = putStrLn
    $ unlines [ [ b ! (i, j) | j <- [jMin .. jMax] ] | i <- [iMin .. iMax] ]
    where ((iMin, jMin), (iMax, jMax)) = bounds b

isCleared :: Board -> Bool
isCleared b = and [ b ! c /= 'x' | c <- indices b ]

transforms :: Board -> [(Board, [Int])]
transforms b0 = aux [] [] [(b0, [])] (S.singleton b0) S.empty where
    aux [] [] [] _ _
        = []
    aux [] [] bps bset bcset
        = aux [] (map takes' bps) [] bset bcset
    aux [] bcnpss bps bset bcset
        = aux (concat $ reverse bcnpss) [] bps bset bcset
    aux bcnps @ ((b, c, n, p) : bcnps') bcnpss bps bset bcset
        | S.notMember b bset
            = (b, n : p) : aux bcnps bcnpss ((b, n : p) : bps) (S.insert b bset) bcset
        | S.notMember (b, c) bcset
            = aux bcnps' (moves' (b, c, n, p) : bcnpss) bps bset (S.insert (b, c) bcset)
        | otherwise
            = aux bcnps' bcnpss bps bset bcset
    moves' (b, c, n, p) = [ (b', c', n + 1, p) | (b', c') <- moves (b, c) ]
    takes' (b, p) = [ (b, c, 0, p) | c <- pieces b ]

pieces :: Board -> [Coord]
pieces b = [ c | c <- indices b , b ! c /= '.' ]

moves :: (Board, Coord) -> [(Board, Coord)]
moves (b, c) = catMaybes [ aux d 1 | d <- dirs ] where
    aux d n
        | not $ inRange (bounds b) (toward d n)
            = Nothing
        | n == 1 && b ! toward d n == '.'
            = Nothing
        | b ! toward d n /= '.'
            = aux d (n + 1)
        | otherwise
            = Just (b', toward d n)
        where b' = b // ((c, '.') : (toward d n, b ! c)
                 : [ (toward d i, invert (b ! toward d i)) | i <- [1 .. n - 1] ])
    toward d n = (fst c + n * fst d, snd c + n * snd d)
    invert 'o' = 'x'
    invert 'x' = 'o'
    invert _   = error "move.invert"

dirs :: [Coord]
dirs = [ (i, j) | i <- [-1 .. 1] , j <- [-1 .. 1] , i /= 0 || j /= 0 ]

allMap :: [a] -> [b] -> [[(a, b)]]
allMap [] _
    = [[]]
allMap (x : xs) ys
    = [ (x, y) : m | y <- ys , m <- allMap xs ys ]
