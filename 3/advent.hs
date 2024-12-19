import Text.Read (readMaybe)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let check :: String -> Int
        check ('d':'o':'n':cs) = check $ dropWhile ('d' /=) cs -- PART 2
        check ('m':'u':'l':cs) = do
            let (mul, cs') = break (')' ==) cs
            case (readMaybe (mul ++ ")") :: Maybe (Int, Int)) of
                Just (n, n') -> n * n' + check cs'
                Nothing      -> check cs
        check (_:cs) = check cs
        check _ = 0
    print $ check input
    pure ()