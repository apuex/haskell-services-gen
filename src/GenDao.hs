{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module GenDao (gen) where

import           System.IO
import           System.Exit
import           System.Environment
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
    printf "%s: %s\n" (CL.paddingRight ("GenDao"::String) 12) file

