import FlipIt (isClear, parse, printBoard, printBoardTrack, transforms)
import qualified System.Environment (getArgs)

main :: IO ()
main = do
    b0 <- fmap parse . (=<<) readFile . fmap head $ System.Environment.getArgs
    printBoard b0
    printBoardTrack . head . filter (isClear . fst) . transforms $ b0
