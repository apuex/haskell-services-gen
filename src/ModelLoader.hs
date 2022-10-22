{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module ModelLoader where

import           System.IO              (stderr)
import           System.Exit
import           Data.List
import           Data.Maybe             (fromMaybe)
import           Text.Printf
import qualified Data.Map               as M
import           Prelude                hiding (readFile, writeFile)
import           Text.Hamlet.XML
import           Text.XML
import           Text.Shakespeare.Text
import qualified Data.Text.Lazy.IO      as TLIO
import qualified Data.Text              as T
import           Control.Monad          (forM_)
import qualified CmdLine                as CL

data Field = Field
    { fieldNo      :: Int
    , fieldName    :: T.Text
    , fieldType    :: T.Text
    , fieldComment :: T.Text
    } deriving Show

data Version = Version
    { versionNo      :: Int
    , versionFields  :: [Field]
    , versionComment :: T.Text
    } deriving Show

data PrimaryKey = PrimaryKey
    { primaryKeyFields  :: [Field]
    , primaryKeyComment :: T.Text
    } deriving Show

data ForeignKey = ForeignKey
    { foreignKeyFields  :: [Field]
    , foreignKeyComment :: T.Text
    } deriving Show

data EnumValue = EnumValue
    { enumValueId      :: Int
    , enumValueName    :: T.Text
    , enumValueVersion :: Int
    , enumValueComment :: T.Text
    } deriving Show

data Enumerate = Enumerate
    { enumerateName           :: T.Text
    , enumerateGenerateCode   :: Bool 
    , enumerateFields         :: [Field]
    , enumeratePrimaryKey     :: Maybe PrimaryKey
    , enumerateValues         :: [EnumValue]
    , enumerateComment        :: T.Text
    } deriving Show

data Entity = Entity
    { entityName             :: T.Text
    , entityIsAggregateRoot  :: Bool 
    , entityGenerateCode     :: Bool 
    , entityComment          :: T.Text
    , entityFields           :: [Version]
    , entityPrimaryKey       :: Maybe PrimaryKey
    , entityForeignKeys      :: [ForeignKey]
    } deriving Show

data Model = Model
    { modelName         :: T.Text
    , modelVersion      :: T.Text
    , modelMaintainer   :: T.Text
    , modelPackage      :: T.Text
    , modelDbSchema     :: T.Text
    , modelJournalTable :: T.Text
    , modelComment      :: T.Text
    , modelEnums        :: [Enumerate] 
    , modelEntities     :: [Entity] 
    } deriving Show


enumerateChild :: ([Field], Maybe PrimaryKey, [EnumValue])
               -> Node 
               -> ([Field], Maybe PrimaryKey, [EnumValue])
enumerateChild (fields, pk, values) x = case x of
    NodeElement e -> case e of
        --Element "field" attrs children ->
        --Element "primaryKey" attrs children ->
        --Element "values" attrs children ->
        _ -> (fields, pk, values)
    _ -> (fields, pk, values)


entityChild :: ([Version], Maybe PrimaryKey, [ForeignKey])
            -> Node
            -> ([Version], Maybe PrimaryKey, [ForeignKey])
entityChild (versions, pk, fks) x = case x of
    NodeElement e -> case e of
        --Element "version" attrs children ->
        --Element "primaryKey" attrs children ->
        --Element "foreignKey" attrs children ->
        _ -> (versions, pk, fks)
    _ -> (versions, pk, fks)

modelChild :: ([Enumerate], [Entity])
            -> Node
            -> ([Enumerate], [Entity])
modelChild (enums, entities) x = case x of
    NodeElement e -> case e of
        Element "enumerate" attrs children ->
            let e = Enumerate 
                    { enumerateName         = fromMaybe ""              $ M.lookup "name"     attrs
                    , enumerateGenerateCode = maybe False boolFromText  $ M.lookup "generate" attrs
                    , enumerateComment      = fromMaybe ""              $ M.lookup "comment"  attrs
                    , enumerateFields       = fields 
                    , enumeratePrimaryKey   = pk
                    , enumerateValues       = values
                    }
            in (enums ++ [e], entities)
            where (fields, pk, values) = foldl enumerateChild ([], Nothing, []) children
        Element "entity" attrs children ->
            let e = Entity 
                    { entityName            = fromMaybe ""              $ M.lookup "name"          attrs
                    , entityIsAggregateRoot = maybe False boolFromText  $ M.lookup "aggregateRoot" attrs
                    , entityGenerateCode    = maybe False boolFromText  $ M.lookup "generate"      attrs
                    , entityComment         = fromMaybe ""              $ M.lookup "comment"       attrs
                    , entityFields          = fields 
                    , entityPrimaryKey      = pk
                    , entityForeignKeys     = fks 
                    }
            in (enums, entities ++ [e])
            where (fields, pk, fks) = foldl entityChild ([], Nothing, []) children
        _ -> (enums, entities)
    _ -> (enums, entities)

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
        , modelComment      = fromMaybe ""                  $ M.lookup "comment"      attrs
        , modelEnums        = enums
        , modelEntities     = entities
        }
        where (enums, entities) = foldl modelChild ([], []) children
    _-> Nothing

boolFromText :: T.Text -> Bool
boolFromText ms = case (T.unpack $ T.toLower ms) of
           "true"   -> True
           "false"  -> False
           "1"      -> True
           "0"      -> False
           _        -> False

