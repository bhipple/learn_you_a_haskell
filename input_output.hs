import Control.Exception
import Control.Monad
import Data.List
import System.Environment
import System.IO
import System.IO.Error
import qualified Data.ByteString.Lazy as B

-- Demonstration of the forM function
getColors :: IO ()
getColors = do
    colors <- forM [1,2,3,4] (\a -> do
       putStrLn $ "Which color do you associate with " ++ show a ++ "?"
       color <- getLine
       return color)
    putStrLn "The colors you associate with 1, 2, 3, 4 are: "
    mapM_ putStrLn colors

-- getContents is amazing, because it creates a lazy stream that pipes
-- input until terminated with ctrl+d

-- File IO
-- getConents works the same as hGetContents, but specifies the handle as
-- stdin
readReadme = do
        handle <- openFile "README.md" ReadMode
        contents <- hGetContents handle
        putStr $ "Contents of the readme file:\n" ++ contents

-- If we want to be more concise, we can use readFile:
-- (There's also writeFile and appendFile)
readReadme' = do
        contents <- readFile "README.md"
        putStr $ "Contents of the readme file with readReadme':\n" ++ contents

-- Start this with $ runhaskell input_output.hs [arguments] to see it go!
main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are:"
    mapM_ putStrLn args
    putStrLn "\nThe program name is:"
    putStrLn progName

-- Also in this section: How to work with random number generators

-- Bytestrings and Lazy bytestrings for better performance when reading files
-- These work by loading up 64K worth of data per "cell" in a list, before
-- creating a thunk promise to lazily evaluate the next 64K. This reduces
-- overhead from storing a list as a serious of [Char], each of which has
-- just a tiny Char followed by the thunk promising to evaluate the next
-- char. Strict bytestrings have no laziness at all.
copyFile' :: FilePath -> FilePath -> IO ()
copyFile' source dest = do
        contents <- B.readFile source
        B.writeFile dest contents

-- Exceptions: Pure code can throw exceptions (see head), but they can only
-- be caught by non-pure code. Generally, prefer to use types like Maybe
-- and Either instead of exceptions in pure code, and reserve exceptions
-- for IO code.
readMaybeFile :: FilePath -> IO ()
readMaybeFile f = tryIt f `catch` handler

tryIt :: FilePath -> IO ()
tryIt f = do
        contents <- readFile f
        putStrLn $ "Read file with: " ++ contents

-- Catch file does not exist errors; others, we re-throw to let something
-- else catch it (or crash the program)
handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "Whoops, had some trouble!"
    | otherwise = ioError e

-- There are many other functions that allow us to ask for information
-- about the fields in the exception throw (e.g., to get the filename that
-- we were looking for that caused the isDoesNotExistError).
