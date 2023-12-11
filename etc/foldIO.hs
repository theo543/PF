import Text.Printf (printf)

inputActions :: [IO Int]
inputActions = [readLn, return 10, (readLn :: IO Int) >> putStr "ignored. try again.\n" >> readLn, error "no more IO."]

compose :: IO Int -> IO b -> IO b
compose = (>>) . (>>= putStr . printf "number: %d!\n")

unit :: IO ()
unit = return ()

main :: IO ()
main = foldr compose unit (take 3 inputActions)
