fx (a : b) = do
    print a
    print b

main :: IO ()
main = do
    fx [1, 2, 3]
