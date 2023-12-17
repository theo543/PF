import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef', writeSTRef)
import Text.Read (readMaybe)
import Control.Exception (try, throw)
import GHC.IO.Exception (IOException(IOError))
import System.IO.Error (isEOFError)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (stderr, hPutStrLn)
import GHC.IO (unsafeInterleaveIO)
import GHC.List (foldl')
import Control.Monad (when)

data IterationCommand = Continue | Break
    deriving (Eq)

while :: Monad m => m IterationCommand -> m ()
while action = do
    keepLooping <- (== Continue) <$> action
    when keepLooping $ while action

-- I know this could be done without mutable state, but I wanted to try out ST.
geometricMean :: (Floating a) => [a] -> a
geometricMean list = runST $ do
    len :: STRef s Int <- newSTRef 0
    product :: STRef s a <- newSTRef 0
    iterator :: STRef s [a] <- newSTRef list

    while $ do
        currentIterator <- readSTRef iterator
        case currentIterator of
            [] ->
                return Break
            x:xs -> do
                modifySTRef' len (+ 1)
                modifySTRef' product (+ log x)
                writeSTRef iterator xs
                return Continue

    finalLen <- readSTRef len
    finalProduct <- readSTRef product
    let finalAnswer = finalProduct / fromIntegral finalLen
    return $ exp finalAnswer

geometricMeanPure :: (Floating a) => [a] -> a
geometricMeanPure list = let
    foldStep (sum, len) x = (sum + log x, len + 1)
    (sum, len) = foldl' foldStep (0, 0) list
    in exp (sum / fromIntegral len)

getMaybeLine :: IO (Maybe String)
getMaybeLine = fmap handleErr (try getLine :: IO (Either IOError String))
    where
        handleErr (Left e) | isEOFError e = Nothing
        handleErr (Left e) = throw e
        handleErr (Right str) = Just str

-- set bool to True to use lazy IO to not OOM on large inputs
-- warning: lazy IO is not referentially transparent
-- probably using Data.Vector would be a better idea to solve the OOM problem
-- but I have no idea how to use cabal to install it, and I wanted to try out lazy IO anyway
readDoublesFromStdin :: Bool -> IO [Double]
readDoublesFromStdin shouldUseLazyIO =
    let possiblyUnsafe = if shouldUseLazyIO then unsafeInterleaveIO else id
        readAction = possiblyUnsafe $ do
            line <- getMaybeLine
            case line >>= readMaybe of
                Nothing -> return []
                Just double -> (double :) <$> readAction
    in readAction

putStrStderr :: String -> IO ()
putStrStderr = hPutStrLn stderr

main :: IO ()
main = do
    tic <- getCurrentTime
    args <- getArgs
    (args, isUnsafeEnabled) <- case args of
            "--lazy-io" : args -> putStrStderr "Enabling unsafe lazy IO. Please note that timing cannot be split into parsing and calculation with lazy IO." >> return (args, True)
            args -> return (args, False)
    let (gmFun, gmFunName) = case args of
            ["--pure"] -> (geometricMeanPure, "geometricMeanPure (implemented in pure Haskell)")
            l | l `elem` [[], ["--st"]] -> (geometricMean, "geometricMean (implemented in ST)")
            _ -> error "usage: st (--lazy-io)? [--pure|--st]"
    doubles <- readDoublesFromStdin isUnsafeEnabled
    tic' <- getCurrentTime
    print $ gmFun doubles
    toc <- getCurrentTime
    let totalElapsed = diffUTCTime toc tic
    let elapsed = diffUTCTime toc tic'
    putStrStderr $ "chosen implementation of geometric mean: " ++ gmFunName
    putStrStderr $ "total elapsed: " ++ show totalElapsed
    putStrStderr $ "total elapsed excluding stdin parsing: " ++ show elapsed
