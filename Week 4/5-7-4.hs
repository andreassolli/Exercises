perfects :: Int -> [Int ]
perfects x = [n | n <- [1..x], sum (factors n) == n]
    where factors n = [x | x <- [1..n-1], n `mod` x == 0]

main :: IO ()
main = do 
    print(perfects(500))