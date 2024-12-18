import Data.List (foldl')

main :: IO ()
main = do
    input <- readFile "input.txt"
    let ns = [map (read :: String -> Int) (words ln) | ln <- lines input]
    print (length [True | ds <- map dif ns, bound 1 3 ds || bound (-3) (-1) ds])
    print (length [True | ds <- map (map dif) (map remove ns),
      or [True | d <- ds, bound 1 3 d || bound (-3) (-1) d]])
    pure ()

dif :: [Int] -> [Int]
dif (x:xs@(x':_)) = x' - x:dif xs
dif _ = []

remove :: [Int] -> [[Int]]
remove (x:xs) = xs:map (x:) (remove xs)
remove _ = []

bound :: Int -> Int -> [Int] -> Bool
bound l u = foldl' (\b -> (\n -> (l <= n) && (n <= u) && b)) True