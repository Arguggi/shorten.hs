module Main( main ) where

import Safe
import System.Exit
import System.Console.GetOpt
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

version :: String
version = "0.1.0.0"

defualtMax, defualtMin :: Int
defualtMin = 8
defualtMax = 64

data Options = Options {
    optInput  :: IO B.ByteString,
    optOutput :: B.ByteString -> IO (),
    optMin    :: Int,
    optMax    :: Int
}

defaultOptions :: Options
defaultOptions = Options {
    optInput  = B.getContents,
    optOutput = B.putStr,
    optMin    = defualtMin,
    optMax    = defualtMax
}

options :: [OptDescr (Options -> IO Options)]
options = [
    Option "v" ["version"] (NoArg showVersion)         "show version number",
    Option "h" ["help"]    (NoArg showHelp)            "output file to write",
    Option "i" ["input"]   (ReqArg readInput   "FILE") "input file to read",
    Option "o" ["output"]  (ReqArg writeOutput "FILE") "output file to write",
    Option "m" ["minL"]    (ReqArg readIntm "Int")     "Minimum length",
    Option "M" ["maxL"]    (ReqArg readIntM "Int")     "Maximum length"]

main :: IO ()
main = do
    args <- getArgs
    let ( actions, _ , _ ) = getOpt RequireOrder options args
    opts <- Prelude.foldl (>>=) (return defaultOptions) actions
    let Options { optInput = input,
        optOutput = output,
        optMin = minL,
        optMax = maxL } = opts
    filterShort input minL maxL >>= output

filterShort :: IO B.ByteString -> Int -> Int -> IO B.ByteString
filterShort x minL maxL = do
    mystr <- x
    return $ short mystr
    where short = BC.unlines . filter rightLen . BC.lines
          rightLen str = (B.length str > minL) && (B.length str < maxL)

showVersion :: Options -> IO Options
showVersion _ = do
    putStrLn $ "Version: " ++ version
    exitWith ExitSuccess

showHelp :: Options -> IO Options
showHelp _ = do
    putStrLn "Usage: shorten -i input -o output -m minL -M maxL"
    exitWith ExitSuccess

readInput, writeOutput :: String -> Options -> IO Options
readInput arg opt = return opt { optInput = B.readFile arg }
writeOutput arg opt = return opt { optOutput = B.writeFile arg }

readIntm, readIntM :: String -> Options -> IO Options
readIntm arg opt = return opt { optMin = readDef defualtMin arg}
readIntM arg opt = return opt { optMax = readDef defualtMax arg}
