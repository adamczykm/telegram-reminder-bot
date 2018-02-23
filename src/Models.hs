{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Data.Aeson
import Data.Aeson.TH
import Data.Text
import Data.Time
import Data.Int (Int64)

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Reminder
  chatId Int64
  timeOfReminder UTCTime
  reminderText Text
  schedulingMsgId Int
  deriving Eq Read Show
|]

$(deriveJSON defaultOptions ''Reminder)
