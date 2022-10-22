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

data Enumerate = Enumerate
    { enumValue    :: Int
    , enumName     :: Text
    , enumVersion  :: Int
    } deriving Show

data Field = Field
    { fieldNo      :: Int
    , fieldName    :: Text
    , fieldType    :: Text
    } deriving Show

data Version = Version
    { versionNo     :: Int
    , versionFields :: [Field]
    } deriving Show

data PrimaryKey = PrimaryKey
    { primaryKeyFields :: [Field]
    } deriving Show

data ForeignKey = ForeignKey
    { foreignKeyFields :: [Field]
    } deriving Show

data Entity = Entity
    { entityName         :: Text
    , entityIsAggregate  :: Bool 
    , entityGenerateCode :: Bool 
    , entityFields       :: [Version]
    , entityPrimaryKey   :: PrimaryKey
    , entityForeignKey   :: ForeignKey
    } deriving Show

data Model = Model
    { modelName         :: Text
    , modelVersion      :: Text
    , modelMaintainer   :: Text
    , modelPackage      :: Text
    , modelDbSchema     :: Text
    , modelJournalTable :: Text
    , modelEnums        :: [Enumerate] 
    , modelEntities     :: [Entity] 
    } deriving Show

model :: Element -> Maybe Model
model root = case root of
    Element "model" attrs children ->
        Just Model
        { modelName         = fromMaybe "example-service"   $ M.lookup "name"         attrs
        , modelVersion      = fromMaybe "1.0.0"             $ M.lookup "version"      attrs
        , modelMaintainer   = fromMaybe "xtwxy@hotmail.com" $ M.lookup "maintainer"   attrs
        , modelPackage      = fromMaybe "Example"           $ M.lookup "package"      attrs
        , modelDbSchema     = fromMaybe "dbSchema"          $ M.lookup "dbSchema"     attrs
        , modelJournalTable = fromMaybe "event_journal"     $ M.lookup "journalTable" attrs
        , modelEnums        = []
        , modelEntities     = []
        }
    _-> Nothing
