{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad              (mzero, void, when)
import           Data.Aeson
import qualified Data.Aeson                 as Aeson
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TextIO
import qualified Database.RethinkDB         as R
import           Database.RethinkDB.NoClash

data Message = Message { getText      :: Text
                       , getRecipient :: Text
                       }
             deriving Show

instance FromDatum Message
instance ToDatum Message
instance Expr Message

instance ToJSON Message where
  toJSON (Message text recipient) = object [ "text" .= text
                                           , "recipient" .= recipient
                                           ]

instance FromJSON Message where
  parseJSON (Aeson.Object v) = Message <$>
                               (v .: "text") <*>
                               (v .: "recipient")
  parseJSON _ = mzero


messageTable :: Table
messageTable = table "messages"

messageRecipientIndex :: Index
messageRecipientIndex = Index "message_recipient_index"

insertMessage :: RethinkDBHandle -> Message -> IO ()
insertMessage h m = (void . run' h) $ messageTable # insert m

findMessagesByRecipient :: RethinkDBHandle -> String -> IO (Cursor Message)
findMessagesByRecipient h recipient =
  run h $ (messageTable # getAll messageRecipientIndex [recipient] # changes) ! "new_val"

initSchema :: RethinkDBHandle -> Database -> IO ()
initSchema h database = do
  tables <- (run h $ tableList database) :: IO [String]
  when ("messages" `elem` tables) $
    (void . run' h) $ messageTable # tableDrop
  (void . run' h) $ messageTable # tableCreate
  (void . run' h) $ messageTable # indexCreate "message_recipient_index" (! "recipient")
  (void . run' h) $ messageTable # indexWait []


main :: IO ()
main = do
  h <- R.connect "localhost" 28015 Nothing
  let database = db "test"
  let _ = R.use database h
  initSchema h database

  insertMessage h (Message "Bonjour" "jb")
  insertMessage h (Message "Comment ça va" "jb")
  insertMessage h (Message "bien" "david")

  messageCursor <- findMessagesByRecipient h "jb"
  each messageCursor $ \ m -> TextIO.putStrLn (getText m)
  R.close h
