{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Main(main) where

import           System.Console.GetOpt
import           System.IO
import           System.Exit
import           System.Environment
import           Data.List
import           Data.Maybe (fromMaybe)
import           Text.Printf
import qualified Data.Map        as M
import           Prelude         hiding (readFile, writeFile)
import           Text.Hamlet.XML
import           Text.XML
import           Text.Shakespeare.Text
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Text (Text)
import           Control.Monad (forM_)

main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs
    (opts, files) <- compileOpts progName args
    print opts
    print files

data Options = Options
    { generateAll  :: Bool
    , printHelp    :: Bool
    } deriving Show

defaultOptions = Options
    { generateAll  = False
    , printHelp  = False
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
   [Option ['a']    ["generate-all"] (OptArg ((\ o opts -> opts { generateAll = boolFromMaybe o }) . fromMaybe "true") "BOOL") "generate all"
   , Option ['h']    ["help"] (NoArg (\ opts -> opts { printHelp = True })) "print this help message"
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
    where header = printf "Usage: %s [OPTION...] files..." progName

