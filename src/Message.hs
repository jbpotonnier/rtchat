{-# LANGUAGE OverloadedStrings #-}

module Message
       (
         Message (..)
       , initSchema
       , insertMessage
       , findMessagesByRecipient
       , findMessagesByRecipientChanges
       ) where

import           Control.Monad              (mzero, void, when)
import           Data.Aeson
import qualified Data.Aeson                 as Aeson
import           Data.Text                  (Text)
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

insertMessage :: Message -> RethinkDBHandle -> IO ()
insertMessage m h = (void . run' h) $ messageTable # insert m

findMessagesByRecipient :: Text -> RethinkDBHandle -> IO [Message]
findMessagesByRecipient recipient h =
  run h $ findMessagesByRecipientQuery recipient

findMessagesByRecipientChanges :: Text -> RethinkDBHandle -> IO (Cursor Message)
findMessagesByRecipientChanges recipient h =
  run h $ (findMessagesByRecipientQuery recipient # changes) ! "new_val"

findMessagesByRecipientQuery :: Text -> ReQL
findMessagesByRecipientQuery recipient =
   messageTable # getAll messageRecipientIndex [recipient]

initSchema :: RethinkDBHandle -> Database -> IO ()
initSchema h database = do
  tables <- (run h $ tableList database) :: IO [String]
  when ("messages" `elem` tables) $
    (void . run' h) $ messageTable # tableDrop
  (void . run' h) $ messageTable # tableCreate
  (void . run' h) $
    messageTable # indexCreate "message_recipient_index" (! "recipient")
  (void . run' h) $ messageTable # indexWait []
