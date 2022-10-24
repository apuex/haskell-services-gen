{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Main(main) where

import           System.Console.GetOpt
import           System.IO
import           System.Environment
import           Control.Monad (when)
import qualified Data.Map          as M
import qualified GenXML            as G
import qualified CmdLine           as CL


main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs
    (opts, files) <- CL.compileOpts progName args
    if CL.printVersion opts
        then putStrLn $ CL.versionInfo progName
        else if null files
        then do
            hPutStrLn stderr "No files specified."
            hPutStr stderr (usageInfo (CL.helpHeader progName) CL.options)
        else mapM_ (G.gen opts) files

