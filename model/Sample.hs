{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Sample where

import qualified Metadata               as Meta
import qualified Data.List              as L
import qualified Data.Text              as T

dataObjId = Meta.Int32Field
    { Meta.fieldName    = "dataObjId"
    , Meta.fieldSize    = Nothing
    , Meta.fieldComment = "数据对象ID"
    }

dataObjType = Meta.EnumerateField
    { Meta.fieldName    = "dataObjType"
    , Meta.fieldType    = "DataObjType"
    , Meta.fieldSize    = Nothing
    , Meta.fieldComment = "数据对象ID"
    }

boolValue = Meta.Int32Field
    { Meta.fieldName    = "boolValue"
    , Meta.fieldSize    = Nothing
    , Meta.fieldComment = "Bool数据对象值"
    }

analogValue = Meta.Float64Field
    { Meta.fieldName    = "analogValue"
    , Meta.fieldComment = "模拟数据对象值"
    }

stringValue = Meta.StringField
    { Meta.fieldName    = "stringValue"
    , Meta.fieldSize    = Just 256
    , Meta.fieldComment = "字符串数据对象值"
    }

updateTime = Meta.UInt32Field
    { Meta.fieldName    = "updateTime"
    , Meta.fieldSize    = Nothing
    , Meta.fieldComment = "更新时间，32位time_t"
    }

updateTime64 = Meta.UInt32Field
    { Meta.fieldName    = "updateTime"
    , Meta.fieldSize    = Nothing
    , Meta.fieldComment = "更新时间，32位time_t"
    }

alarmBegin = Meta.UInt32Field
    { Meta.fieldName    = "alarmBegin"
    , Meta.fieldSize    = Nothing
    , Meta.fieldComment = "告警发生时间，32位time_t"
    }

alarmEnd = Meta.UInt32Field
    { Meta.fieldName    = "alarmEnd"
    , Meta.fieldSize    = Nothing
    , Meta.fieldComment = "告警结束时间，32位time_t"
    }

alarmBegin64 = Meta.UInt64Field
    { Meta.fieldName    = "alarmBegin64"
    , Meta.fieldSize    = Nothing
    , Meta.fieldComment = "告警发生时间，64位time_t"
    }

alarmEnd64 = Meta.UInt64Field
    { Meta.fieldName    = "alarmEnd64"
    , Meta.fieldSize    = Nothing
    , Meta.fieldComment = "告警结束时间，64位time_t"
    }

updateDataObjValueFieldVersion1 = Meta.Version
    { Meta.versionNo      = 1
    , Meta.versionFields  =
        [ dataObjId
        , dataObjType
        , updateTime
        , boolValue
        , analogValue
        , stringValue
        ]
    , Meta.versionComment = "2004年版。"
    }

updateDataObjValueFieldVersion2 = Meta.Version
    { Meta.versionNo      = 2
    , Meta.versionFields  =
        [ updateTime64
        ]
    , Meta.versionComment = "2022年版。"
    }

updateDataObjValue = Meta.Message
    { Meta.entityId     = 1
    , Meta.entityName   = "UpdateDataObjValue"
    , Meta.entityFields =
        [ updateDataObjValueFieldVersion1
        , updateDataObjValueFieldVersion2
        ]
    }

