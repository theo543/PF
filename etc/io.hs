import Control.Monad (when, liftM)
main = let readNumber = readLn :: IO Int
           printIt :: Int -> IO Int
           printIt nr = print ("You said: " ++ show nr) >> return nr
           isEvenM :: Int -> IO Bool
           isEvenM x = return $ even x
           againIf :: Bool -> IO ()
           againIf isEven = when isEven main
       in readNumber >>= printIt >>= isEvenM >>= againIf
