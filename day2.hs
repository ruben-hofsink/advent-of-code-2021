main :: IO ()
main = do
    contents <- readFile "./input/day2.txt"
    let cleaned = [words line | line <- lines contents]
    let end1 = process1 (0,0) cleaned
    print (fst end1 * (snd end1))
    let end2 = process2 (0,0,0) cleaned
    print (multTriple end2)


process1 :: (Int, Int) -> [[String]] -> (Int, Int)
process1 cs [] = cs
process1 cs (i:is) = process1 (move1 cs (i !! 0) (read (i !! 1) :: Int)) is

process2 :: (Int, Int, Int) -> [[String]] -> (Int, Int, Int)
process2 cs [] = cs
process2 cs (i:is) = process2 (move2 cs (i !! 0) (read (i !! 1) :: Int)) is

move1 :: (Int, Int) -> String -> Int -> (Int, Int)
move1 (hor,dep) dir delta = 
    case dir of
        "forward" -> (hor + delta, dep)
        "down" -> (hor, dep + delta)
        "up" -> (hor, dep - delta)

move2 :: (Int, Int, Int) -> String -> Int -> (Int, Int, Int)
move2 (hor, dep, aim) instr delta = 
    case instr of
        "forward" -> (hor + delta, dep + (aim * delta), aim) 
        "down" -> (hor, dep, aim + delta)
        "up" -> (hor, dep, aim - delta)

multTriple :: (Int, Int, Int) -> Int
multTriple (a,b,c) = a * b
