module CmdLine where

import           System.Console.GetOpt
import           System.IO
import           System.Exit
import           Data.Maybe (fromMaybe)
import           Text.Printf


data Options = Options
    { generateAll  :: Bool
    , outputDir    :: String
    , printHelp    :: Bool
    , debug        :: Bool
    , printVersion :: Bool
    , verbose      :: Bool
    } deriving Show

defaultOptions = Options
    { generateAll  = False
    , outputDir    = "dist"
    , printHelp    = False
    , debug        = False
    , printVersion = False
    , verbose      = False
    }

boolFromMaybe:: String -> Bool
boolFromMaybe ms = case ms of
           "true"   -> True
           "false"  -> False
           "1"      -> True
           "0"      -> False
           _        -> False


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['a'] ["generate-all"]
       (OptArg ((\ o opts -> opts { generateAll = boolFromMaybe o }) . fromMaybe "true") "BOOL")
       "generate all"
   , Option ['d'] ["debug"]
       (OptArg ((\ o opts -> opts { debug = boolFromMaybe o }) . fromMaybe "true") "BOOL")
       "print out debug information"
   , Option [] ["verbose"]
       (NoArg (\ opts -> opts { verbose = True }))
       "print out verbose information"
   , Option ['o'] ["output-dir"]
       (OptArg ((\ o opts -> opts { outputDir = o }) . fromMaybe "") "DIR")
       "output directory for generated files"
   , Option ['v'] ["version"]
       (NoArg (\ opts -> opts { printVersion = True }))
       "print version number"
   , Option ['h'] ["help"]
       (NoArg (\ opts -> opts { printHelp = True }))
       "print this help message"
   ]

compileOpts :: PrintfArg t => t -> [String] -> IO (Options, [String])
compileOpts progName argv = case getOpt Permute options argv of
    (o,n,[]  ) -> do
        let opts = foldl (flip id) defaultOptions o
        if printHelp opts
            then do hPutStr stderr (usageInfo header options)
                    exitSuccess
            else return (opts, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = helpHeader progName

versionNumber :: String
versionNumber = "0.1.0.0"

versionInfo :: PrintfArg t => t -> String
versionInfo progName = printf "%s-%s by Wangxy <xtwxy@hotmail.com> licensed with Mozilla Public License Version 2.0." progName versionNumber

helpHeader :: PrintfArg t => t -> String
helpHeader progName = printf "%s\nUsage: %s [OPTION...] files..." (versionInfo progName) progName

paddingLeft :: String -> Int -> String
paddingLeft s n =
    printf fmt s where
        fmt = printf "%%%d.%ds" n n

paddingRight :: String -> Int -> String
paddingRight s n =
    reverse rpl where
        rs  = reverse s
        fmt = printf "%%%d.%ds" n n
        rpl = printf fmt rs
