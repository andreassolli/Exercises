libSum :: [Int] -> Int
libSum [_]    = 0
libSum (x:y) = sum [x..y]

libTake :: ([Int], Int) -> [Int]             
libTake ([], _)    = []          
libTake (x:xs, n)  = x : libTake (xs, n-1) 

libLast :: [Int] -> Int
libLast [_]    = 0
libLast [x, y] = last [x..y]

main :: IO()
main = do
    print(libSum [1..10])
    print(libTake ([1..10], 2))
    print(libLast [1..10])
