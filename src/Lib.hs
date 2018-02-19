{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}


module Lib
    (
      startApp
    )
where

import Control.Concurrent
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Network.HTTP.Client      (Manager, newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           System.Environment
import Data.Time
import           Version                  (Version, apiVersion)
import           Web.Telegram.API.Bot
-- import qualified Paths_orly_bookstore_bot (version) as P
import           Network.Wai.Handler.Warp
import           Servant
import           Control.Monad.IO.Class (liftIO)
-- import Data.Int (Int64)


import           Control.Monad.Logger (runStderrLoggingT)


import           Database.Persist.Sqlite ( Entity, ConnectionPool, createSqlitePool, unSqlBackendKey
                                         , runSqlPool, runSqlPersistMPool, selectList
                                         , runMigration, (>=.),(<=.),(==.)
                                         , insert, entityVal)

import Models


-- data Version = MkVersion { version :: Text } deriving (Show, Generic)
-- instance ToJSON Version

----------------- API ------------------------
type BotAPI = "version" :> Get '[JSON] Version
         :<|> "webhook" -- maps to /webhook/<secret_token>
              :> Capture "secret" Text
              :> ReqBody '[JSON] Update
              :> Post '[JSON] ()



botApi :: Proxy BotAPI
botApi = Proxy



--------------- Bot Monad &  Config -------------------------
data BotConfig = MkBotConfig
  { telegramToken :: Token
  , paymentsToken :: Text
  , manager       :: Manager
  , dbConnectionPool :: ConnectionPool
  }

newtype Bot a = MkBot
    { runBot :: ReaderT BotConfig Handler a
    } deriving ( Functor, Applicative, Monad, MonadIO,
                 MonadReader BotConfig, MonadError ServantErr)


startApp :: IO ()
startApp = do
  ------------------- temp bot config ----------------------
  putStrLn "starting app"
  env <- getEnvironment
  manager' <- newManager tlsManagerSettings

  ------------------ setup database ------------------------
  dbPool <- setupDbConnectionPool

  ------------------ build config --------------------------
  let telegramToken' = fromJust $ lookup "TELEGRAM_TOKEN" env
      paymentsToken' = fromJust $ lookup "PAYMENTS_TOKEN" env
      config = MkBotConfig
        { telegramToken = Token $ T.pack $ "bot" <> telegramToken'
        , paymentsToken = T.pack paymentsToken'
        , manager = manager'
        , dbConnectionPool = dbPool
        }

  --------------  run reminding task ------------------
  void $ forkIO $ runRemindingTask config

  --------------  run servant application ------------------
  run 8080 $ serve botApi $ initBotServer config



----------------- Bot logic ----------------------------

data BotCommand = forall a. MkBotCommand (Bot (TelegramClient a))

runBotCommand :: BotCommand -> Bot ()
runBotCommand (MkBotCommand bmr) = do
    MkBotConfig{..} <- ask
    mr <- bmr
    void $ liftIO $ runClient mr telegramToken manager

handleMessage :: Message -> Bot ()
handleMessage msg = case delegateCommand msg of
    Nothing  -> error "Command not handled. TODO: log instead of crash"
    Just cmd -> runBotCommand cmd

delegateCommand :: Message -> Maybe BotCommand
delegateCommand msg = onCommand (text msg)
  where
    onCommand (Just (fmap T.strip . T.stripPrefix "/add_reminder" -> Just _)) = addNewReminderCommand msg
    onCommand (Just (fmap T.strip . T.stripPrefix "/list_reminders" -> Just _)) = listRemindersCommand msg
    onCommand _ = helpMessage msg

helpMessage ::  Message -> Maybe BotCommand
helpMessage msg = do
    let chatId = ChatId $ chat_id $ Web.Telegram.API.Bot.chat msg
    return $ MkBotCommand $ return $ sendMessageM $ sendMessageRequest chatId $ T.unlines
      [ "/help                                     - show this message"
      , "/add_reminder timestamp_utc reminder_text - add reminder."
      , "/list_reminders                           - list reminders for this chat."
      ]

addNewReminderCommand :: Message -> Maybe BotCommand
addNewReminderCommand msg = do
  msgTxt <- text msg

  -- parsing
  let (_, rest) = T.breakOn " " msgTxt
      chatId = chat_id $ Web.Telegram.API.Bot.chat msg
      (timestampStr, reminderStr) = T.breakOn "|" rest
      -- timestamp = parseReminderTime timestampStr


  return $ MkBotCommand $ do
    pool <- dbConnectionPool <$> ask
    timestamp <- liftIO $ addUTCTime (realToFrac 60) <$> getCurrentTime
    reminderId <- liftIO $ flip runSqlPersistMPool pool $ insert $ Reminder chatId timestamp reminderStr
    let returned_msg = T.append "Added new reminder with ID: " (T.pack $ show $ unSqlBackendKey $ unReminderKey reminderId)
    return $ sendMessageM $ sendMessageRequest (ChatId chatId) returned_msg

  where
    parseReminderTime :: Text -> UTCTime
    parseReminderTime _ = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)


listRemindersCommand :: Message -> Maybe BotCommand
listRemindersCommand msg = do
  -- parsing
  let chatId = chat_id $ Web.Telegram.API.Bot.chat msg

  return $ MkBotCommand $ do
    pool <- dbConnectionPool <$> ask
    reminders :: [Entity Reminder]  <- liftIO $ flip runSqlPersistMPool pool $ selectList [ ReminderChatId ==. chatId] []
    let returned_msg = T.unlines $ "Reminders for this chat:" : (T.pack . show . entityVal <$> reminders)
    return $ sendMessageM $ sendMessageRequest (ChatId chatId) returned_msg

--------------- reminding task -----------------------

runRemindingTask :: BotConfig -> IO ()
runRemindingTask MkBotConfig{..} = forever $ do
  timeNow <- getCurrentTime
  let seconds = 10
      timeRange = fromIntegral (seconds `div` 2)
  
  threadDelay $ seconds * 1000000
  -- get all reminders
  reminders :: [Entity Reminder]  <- liftIO $ flip runSqlPersistMPool dbConnectionPool $ selectList [ReminderTimeOfReminder >=. addUTCTime (realToFrac (- timeRange)) timeNow , ReminderTimeOfReminder <=. addUTCTime (realToFrac timeRange) timeNow] []
  
  mapM sendReminder reminders

  where
    sendReminder (entityVal -> (Reminder cId _ msg)) = liftIO $ runClient (sendMessageM (sendMessageRequest (ChatId cId) msg)) telegramToken manager


------------- Server ------------------------------

botServer :: ConnectionPool -> ServerT BotAPI Bot
botServer _ = return apiVersion :<|> handleWebhook
  where
    handleWebhook :: Text -> Update -> Bot ()
    handleWebhook secret update = do
      Token token <- asks telegramToken
      if EQ == compare secret token
        then handleUpdate update
        else throwError err403

    handleUpdate :: Update -> Bot ()
    handleUpdate update =
      case update of
        Update { message = Just msg } -> handleMessage msg
        _ -> liftIO $ putStrLn $ "Handle update failed. " ++ show update


initBotServer :: BotConfig -> Server BotAPI
initBotServer config = hoistServer botApi (bot2handler config) (botServer (dbConnectionPool config))
    where
      -- bot2handler :: BotConfig -> Bot a -> Handler a
      bot2handler cfg = flip runReaderT cfg . runBot


------------- DB Connection ----------------------

dbFilePath :: Text
dbFilePath = "sqlite.db"


setupDbConnectionPool :: IO ConnectionPool
setupDbConnectionPool = do
  pool <- runStderrLoggingT $ createSqlitePool dbFilePath 5
  runSqlPool (runMigration migrateAll) pool
  return pool
