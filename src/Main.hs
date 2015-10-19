{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Message                    (Message (..),
                                             findMessagesByRecipient,
                                             initSchema, insertMessage)

import qualified Data.Text.IO               as TextIO
import qualified Database.RethinkDB         as R
import           Database.RethinkDB.NoClash

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
