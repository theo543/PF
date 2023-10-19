import Control.Monad (when)
main = do
    x :: Int <- readLn
    print $ "You said: " ++ show x
    y :: Int <- readLn
    print $ "You said: " ++ show x ++ " and " ++ show y
    when (even x || even y) main
