import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import Control.Exception (try)
import System.IO.Error (IOError, isEOFError)
import System.Directory.Internal.Prelude (getArgs, exitFailure, hFlush, stdout)

prompt :: IO ()
prompt = do
    putStr "Enter a line: "
    hFlush stdout

{-
Strict IO reading lines from stdin until an IOError (likely EOF) is thrown.
This is 100% safe and referentially transparent, but in this case it will freeze the program until EOF is sent.
Since the IO in main can't proceed until the IO here is done, no O(1)-space processing optimizations can be done, forcing the list to be stored in memory.
-}
getLines :: IO [String]
getLines = do
    prompt
    line <- try @IOError getLine
    case line of
        Left _ -> return []
        Right line -> do
            rest <- getLines
            return (line : rest)

{-
Lazy IO reading lines from stdin until EOF is sent. Other errors are not handled.

This uses the unsafeInterleaveIO function, which essentially removes the requirement that this IO's effects must be done before main can proceed.
The result is still in IO, unlike unsafePerformIO (which would just cause an infinite list of the first line), but that doesn't mean it's safe.
Because the value you get depends on effects which were not performed when the value was created, pure code evaluating the value will trigger the effects.
If we were using files, we'd also need to manage closing the file handle, including when the list is discarded partway through, possibly with a finalizer.

Alternatives to lazy IO include doing everything in IO, or using streaming libraries like streaming, pipes, or conduit. Using libraries is probably the best option.
-}
getLinesLazy :: IO [String]
getLinesLazy = unsafeInterleaveIO $ do
    prompt
    line <- try @IOError getLine
    case line of
        Left err | isEOFError err -> return []
                 | otherwise -> ioError err
        Right line -> do
            rest <- getLinesLazy
            return (line : rest)

main :: IO ()
main = do
    args <- getArgs
    chosen <- case args of
        ["--strict-io"] -> putStr "Strict IO chosen. You will not see any line lengths until EOF is sent.\n" >> return getLines
        ["--lazy-io"] -> putStr "Lazy IO chosen. You will see line lengths as they're entered.\n" >> return getLinesLazy
        _ -> putStr "Usage: lazyio [--strict-io|--lazy-io]\n" >> exitFailure
    putStr "Note: To send EOF, press Ctrl+D on Linux or Ctrl+Z on Windows.\n"
    lines <- chosen
    mapM_ (print . length) lines
    putStr "Done.\n"
