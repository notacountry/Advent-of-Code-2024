import Data.Char
import Data.List

main :: IO ()
main = do
    input <- readFile "input.txt"
    let (ns, ns') = unzip $ map lineToInt $ lines input
    print(distance (sort ns, sort ns'))
    pure ()

lineToInt :: String -> (Int, Int)
lineToInt cs = (read n :: Int, read n' :: Int)
    where
      (n, rest) = span (isNumber) cs
      n' = snd $ break (isNumber) rest

distance :: ([Int], [Int]) -> Int
distance = uncurry ((sum .) . zipWith ((abs .) . (-)))