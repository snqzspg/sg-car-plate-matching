lessThanFive :: Int -> Bool
lessThanFive n = n < 5

main :: IO ()
main = do
    putStrLn $ [1..8] >>= (\x -> '\n' : show (lessThanFive x))
