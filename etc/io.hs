import Control.Monad (when)
main =
    (readLn :: IO Int) >>=
    \x -> print ("You said: " ++ show x) >>
    (readLn :: IO Int) >>=
    \y -> print ("You said: " ++ show x ++ " and " ++ show y) >>
    when (even x || even y) main
