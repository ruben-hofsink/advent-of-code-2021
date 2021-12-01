main :: IO ()
main = do
    contents <- readFile "./input/day1.txt"
    let cleaned = [read line :: Int | line <- lines contents]
    let solve1 = countIncreasing cleaned
    print solve1
    let solve2 = countIncreasing (map (\x -> sum x) (slidingWindow 3 cleaned))
    print solve2


countIncreasing :: [Int] -> Int
countIncreasing xs = sum [1 | (a,b) <- zip xs (tail xs), b > a]

slidingWindow :: Int -> [Int] -> [[Int]]
slidingWindow size xs = 
    case (length xs) >= size of
        True -> [take size xs] ++ (slidingWindow size (tail xs))
        False -> []