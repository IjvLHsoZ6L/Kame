import Control.Applicative
import Data.Array
-- import Data.Set

main :: IO ()
main = do
    b <- read <$> readFile "problem.txt" :: IO Board
    print b
    print $ move b (1, 0) (3, 0)
    print $ move (move b (1, 0) (3, 0)) (3, 0) (3, 3)
    print $ move (move (move b (1, 0) (3, 0)) (3, 0) (3, 3)) (3, 3) (0, 0)

data State = Blank | Obverse | Reverse

fromState :: State -> Char
fromState Blank   = '.'
fromState Obverse = 'o'
fromState Reverse = 'x'

toState :: Char -> State
toState '.' = Blank
toState 'o' = Obverse
toState 'x' = Reverse
toState _   = error "unknown charcter"

data Board = Board
    { height :: Int
    , width  :: Int
    , states :: Array (Int, Int) State
    }

instance Show Board where
    show b
        = unlines
            [
                [ fromState (states b ! (i, j))
                | j <- range (0, width b - 1)
                ]
            | i <- range (0, height b - 1)
            ]

instance Read Board where
    readsPrec _ s = [(b, "")] where
        b = Board
            { height = h
            , width  = w
            , states = array ((0, 0), (h - 1, w - 1)) ies
            }
        h = length ss
        w = length (head ss)
        ss = lines s
        ies = [ ((i, j), toState (ss !! i !! j))
            | (i, j) <- range ((0, 0), (h - 1, w - 1)) ]

exists :: Board -> (Int, Int) -> Bool
exists b p = case states b ! p of
    Blank -> False
    _     -> True

canMove :: Board -> (Int, Int) -> (Int, Int) -> Bool
canMove b (i0, j0) (i1, j1)
    | exists b (i1, j1)
        = False
    | abs (i1 - i0) <= 1 && abs (j1 - j0) <= 1
        = False
    | i0 == i1
        = and [ exists b (i0, j) | j <- js ]
    | j0 == j1
        = and [ exists b (i, j0) | i <- is ]
    | i1 - i0 == j1 - j0
        = and [ exists b p | p <- zip is js ]
    | i1 - i0 == j0 - j1
        = and [ exists b p | p <- zip is (reverse js) ]
    | otherwise
        = False
    where
        is = range (min i0 i1 + 1, max i0 i1 - 1)
        js = range (min j0 j1 + 1, max j0 j1 - 1)

move :: Board -> (Int, Int) -> (Int, Int) -> Board
move b (i0, j0) (i1, j1) = b { states = states b // updates } where
    updates = ((i0, j0), Blank) : ((i1, j1), states b ! (i0, j0))
            : [ (p, turn $ states b ! p) | p <- segment ]
    segment
        | i0 == i1
            = zip (repeat i0) js
        | j0 == j1
            = zip is (repeat j0)
        | i1 - i0 == j1 - j0
            = zip is js
        | i1 - i0 == j0 - j1
            = zip is (reverse js)
        | otherwise
            = error "error"
    is = range (min i0 i1 + 1, max i0 i1 - 1)
    js = range (min j0 j1 + 1, max j0 j1 - 1)
    turn Obverse = Reverse
    turn Reverse = Obverse
    turn _       = error "error"
