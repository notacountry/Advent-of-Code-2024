import Data.Char
import Data.List
import Data.Maybe

main :: IO ()
main = do
    input <- readFile "input.txt"
    let (ns, ns') = unzip $ map lineToInt $ lines input
    print(((sum .) . zipWith ((abs .) . (-))) (sort ns) (sort ns'))
    print(similarity ns (createMap ns'))
    pure ()

lineToInt :: String -> (Int, Int)
lineToInt cs = let [l, r] = map (read :: String -> Int) (words cs) in (l, r)

createMap :: [Int] -> [(Int, Int)]
createMap (x:xs) = case (findIndex (\n -> x == fst n) oldMap) of
    Nothing -> (x, 1) : oldMap
    Just i  ->
        let (first, (_, n):last) = splitAt i oldMap
        in first ++ (x, n + 1) : last
  where oldMap = createMap xs
createMap _ = []

similarity :: [Int] -> [(Int, Int)] -> Int
similarity (x:xs) map = case (lookup x map) of
    Nothing -> similarity xs map
    Just n  -> x * n + similarity xs map
similarity _ map = 0