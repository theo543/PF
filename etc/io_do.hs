import Control.Monad (when)
main = do
    nr :: Int <- readLn
    let str :: String = "You said: " ++ show nr
    print str
    when (even nr) main
