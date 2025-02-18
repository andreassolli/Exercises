type Position = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Position -> Position
move North (x, y) = (x    , y + 1)
move West  (x, y) = (x - 1, y    )
move East (x, y) = (x + 1, y)
move South (x, y) = (x, y - 1)

moves :: [Direction] -> Position -> Position
moves [] pos = pos
moves(d:ds) pos = moves ds (move d pos)

main :: IO ()
main = do
    let start = (0,0)
        directions = [North, East, East, South, West]
        finalPosition = moves directions start
    print finalPosition 