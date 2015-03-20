import FlipIt (Board, Coord, isClear, printBoard, transforms)
import Data.Array.Unboxed (array, range)

main :: IO ()
main = sequence_
    [ do
        print m
        printBoard b0
    | b0 <- allBoards
    , let bts = filter (isClear . fst) . takeWhile ((<= 2) . length . snd) . transforms $ b0
    , not $ null bts
    , let m = minimum $ map (sum . map (subtract 1 . length) . snd) bts
    , m >= 8
    ]

bds :: (Coord, Coord)
bds = ((0, 0), (3, 3))

allBoards :: [Board]
allBoards = [ array bds kvs | kvs <- allMaps (range bds) ".x" ]

allMaps :: [a] -> [b] -> [[(a, b)]]
allMaps [] _
    = [[]]
allMaps (x : xs) ys
    = [ (x, y) : kvs | y <- ys , kvs <- allMaps xs ys ]
