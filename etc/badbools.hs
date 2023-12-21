import System.IO.Unsafe (unsafeInterleaveIO)
import Data.IORef (newIORef, readIORef, writeIORef)

-- Gets two entangled booleans, which break the usual rules of Haskell, since order of evaluation isn't supposed to matter.
getBadBools :: IO (Bool, Bool)
getBadBools = do
    flag <- newIORef True
    b1 <- unsafeInterleaveIO $
        readIORef flag
    b2 <- unsafeInterleaveIO $ do
        b <- readIORef flag
        writeIORef flag False
        return b
    return (b1, b2)

cmpForward :: Bool -> Bool -> Bool
cmpForward = (&&)

cmpBackward :: Bool -> Bool -> Bool
cmpBackward = flip (&&)

-- cmpForward and cmpBackward are supposed to be 100% exactly the same thing, but this isn't true when unsafe code is involved... be careful!

main :: IO ()
main = do
    (b1, b2) <- getBadBools
    putStrLn $ "b1 && b2 = " ++ show (cmpForward b1 b2)
    (b1, b2) <- getBadBools
    putStrLn $ "b2 && b1 = " ++ show (cmpBackward b1 b2)
