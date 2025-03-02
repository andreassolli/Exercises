sumSquare :: (Integer, Integer) -> [Integer]
sumSquare (x, y) = [z^2 | z <- [x..y]]

main :: IO ()
main = do
    print (sumSquare (1, 100))