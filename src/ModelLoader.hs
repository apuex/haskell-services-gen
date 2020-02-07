{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module ModelLoader where

import           System.IO (stderr)
import           System.Exit
import           Control.Monad (when)
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
import qualified CmdLine as CL


gen :: CL.Options -> String -> IO ()
gen opts file = do
    printf "%s: %s\n" (CL.paddingRight ("GenMessage"::String) 12) file
    Document prologue root epilogue <- readFile def file
    genMessage opts root

genMessage :: CL.Options -> Element -> IO ()
genMessage opts root = do
    case root of
        Element "model" attrs children ->
            mapM_ (\e -> printf "%s -> %s\n" (nameLocalName (fst e)) (snd e)) (M.toList attrs)
        Element name attrs children -> do
            hPrintf stderr "Invalid root element name: %s\n" (show name)
            exitFailure
