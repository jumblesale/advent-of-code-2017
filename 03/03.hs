data Direction = R | U | L | D
  deriving (Show, Eq)

-- our puzzle input
input :: Int
input = 312051

-- series: R U LL DD RRR UUU LLLL DDDD RRRRR UUUUU LLLLLL..
--         1 1 2  2  3   3   4    4    5     5     6     ..

-- repeat R U L D forever
directionSeq :: [Direction]
directionSeq = concat $ repeat [R, U, L, D]

-- 1, 1, 2, 2, 3, 3, 4, 4..
stepSeq :: [Int]
stepSeq = concatMap (replicate 2) [1..]

-- replicate directions step number of times
directions :: [Direction]
directions = concat $ zipWith replicate stepSeq directionSeq

-- map directions to change in co-ords
directionToFunction :: Direction -> (Int, Int) -> (Int, Int)
directionToFunction R (x, y) = (x+1, y)
directionToFunction U (x, y) = (x, y+1)
directionToFunction L (x, y) = (x-1, y)
directionToFunction D (x, y) = (x, y-1)
-- too much typing
dtf :: Direction -> (Int, Int) -> (Int, Int)
dtf = directionToFunction

pattern :: [((Int, Int), Int)]
pattern =
  -- scanl is foldl but with intermediate steps
  scanl
  -- given (x, y) and the current value, find the next co-ordinate
  -- and increase the value
  (\((x, y), v) d -> (dtf d (x, y), v + 1))
  -- start at location (0, 0) with value 1
  ((0, 0), 1)
  -- directions is an infinite list of [Direction]
  directions

-- keep taking from the pattern until we hit the limit
allSteps :: Int -> [((Int, Int), Int)]
allSteps l = takeWhile (\((_, _), v) -> v <= l) pattern

-- taxi cab distance
tcd :: (Int, Int) -> (Int, Int) -> Int
tcd (x1, y1) (x2, y2) = (abs x1 - x2) + (abs y1 - y2)

-- what's the final location we arrive at?
lastStep :: ((Int, Int), Int)
lastStep = last $ allSteps input

-- convert a location ((x, y), value) to a taxi cab distance
locationToTcd :: ((Int, Int), Int) -> (Int, Int) -> Int
locationToTcd ((x, y), _) origin = tcd (x, y) origin

solution1 :: Int
solution1 = locationToTcd lastStep (0, 0)
