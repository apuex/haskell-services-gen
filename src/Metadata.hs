{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveGeneric #-}
-- since ghc 8.0.1 {-# LANGUAGE Strict #-}
-- since ghc 8.0.1 {-# LANGUAGE StrictData #-}
module Metadata where

import qualified Data.Text              as T
import           GHC.Generics              (Generic)

data Field
    = Int8Field
    { fieldName    :: !T.Text
    , fieldSize    :: !(Maybe Int) -- (0, 8) bits
    , fieldComment :: !T.Text
    }
    | UInt8Field
    { fieldName    :: !T.Text
    , fieldSize    :: !(Maybe Int) -- (0, 8) bits
    , fieldComment :: !T.Text
    }
    | Int16Field
    { fieldName    :: !T.Text
    , fieldSize    :: !(Maybe Int) -- (0, 16) bits
    , fieldComment :: !T.Text
    }
    | UInt16Field
    { fieldName    :: !T.Text
    , fieldSize    :: !(Maybe Int) -- (0, 16) bits
    , fieldComment :: !T.Text
    }
    | Int32Field
    { fieldName    :: !T.Text
    , fieldSize    :: !(Maybe Int) -- (0, 32) bits
    , fieldComment :: !T.Text
    }
    | UInt32Field
    { fieldName    :: !T.Text
    , fieldSize    :: !(Maybe Int) -- (0, 32) bits
    , fieldComment :: !T.Text
    }
    | Int64Field
    { fieldName    :: !T.Text
    , fieldSize    :: !(Maybe Int) -- (0, 64) bits
    , fieldComment :: !T.Text
    }
    | UInt64Field
    { fieldName    :: !T.Text
    , fieldSize    :: !(Maybe Int) -- (0, 64) bits
    , fieldComment :: !T.Text
    }
    | Float32Field
    { fieldName    :: !T.Text
    , fieldComment :: !T.Text
    }
    | Float64Field
    { fieldName    :: !T.Text
    , fieldComment :: !T.Text
    }
    | StringField
    { fieldName    :: !T.Text
    , fieldSize    :: !(Maybe Int) -- Just a Number
    , fieldComment :: !T.Text
    }
    | EnumerateField
    { fieldName    :: !T.Text
    , fieldType    :: !T.Text
    , fieldSize    :: !(Maybe Int) -- Just a Number
    , fieldComment :: !T.Text
    }
    | EntityField
    { fieldName    :: !T.Text
    , fieldType    :: !T.Text
    , fieldSize    :: !(Maybe Int) -- Just a Number
    , fieldComment :: !T.Text
    }
    deriving (Eq, Show, Generic)

data Version = Version
    { versionNo      :: !Int
    , versionFields  :: ![Field]
    , versionComment :: !T.Text
    }
    deriving (Eq, Show, Generic)

data ForeignKey = ForeignKey
    { foreignKeyFields  :: ![Field]
    , foreignKeyComment :: !T.Text
    }
    deriving (Eq, Show, Generic)

data EnumValue = EnumValue
    { enumValueId      :: !Int
    , enumValueName    :: !T.Text
    , enumValueVersion :: !Int
    , enumValueComment :: !T.Text
    }
    deriving (Eq, Show, Generic)

data Enumerate = Enumerate
    { enumerateName    :: !T.Text
    , enumerateFields  :: ![Field]
    , enumerateValues  :: ![EnumValue]
    , enumerateComment :: !T.Text
    }
    deriving (Eq, Show, Generic)

data Entity
    = Message
    { entityId      :: !Int
    , entityName    :: !T.Text
    , entityFields  :: ![Version] -- order by versionNo
    }
    | State 
    { entityId      :: !Int
    , entityName    :: !T.Text
    , entityFields  :: ![Version] -- order by versionNo
    }
    deriving (Eq, Show, Generic)

data Model = Model
    { enums    :: ![Enumerate]
    , entities :: ![Entity]
    }

