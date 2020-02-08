{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module ModelLoader where

import           System.IO (stderr)
import           System.Exit
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

data Model = Model
    { name         :: Text
    , version      :: Text
    , maintainer   :: Text
    , package      :: Text
    , dbSchema     :: Text
    , journalTable :: Text
    } deriving Show

model :: Element -> Maybe Model
model root = case root of
    Element "model" attrs children ->
        Just Model
        { name         = fromMaybe "example-service"   $ M.lookup "name"         attrs
        , version      = fromMaybe "1.0.0"             $ M.lookup "version"      attrs
        , maintainer   = fromMaybe "xtwxy@hotmail.com" $ M.lookup "maintainer"   attrs
        , package      = fromMaybe "Example"           $ M.lookup "package"      attrs
        , dbSchema     = fromMaybe "dbSchema"          $ M.lookup "dbSchema"     attrs
        , journalTable = fromMaybe "event_journal"     $ M.lookup "journalTable" attrs
        }
    _-> Nothing
