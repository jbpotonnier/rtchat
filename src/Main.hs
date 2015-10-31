{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Message                              (Message (..),
                                                       findMessagesByRecipient,
                                                       findMessagesByRecipientChanges,
                                                       insertMessage)

import           Control.Exception                    (bracket)
import qualified Database.RethinkDB                   as R

import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Handler.WebSockets       as WaiWS
import qualified Network.WebSockets                   as WS

import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static
import           Web.Spock

connectToRethinkDB :: IO R.RethinkDBHandle
connectToRethinkDB =
  let database = R.db "test" in do
    h <- R.connect "localhost" 28015 Nothing
    let _ = R.use database h
    return h

dbConn :: PoolOrConn R.RethinkDBHandle
dbConn =
  PCConn (ConnBuilder
          connectToRethinkDB
          R.close
          (PoolCfg 5 5 60)
         )

appMiddleware :: SpockT (WebStateM R.RethinkDBHandle () ()) ()
appMiddleware = do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "static")


appRoutes :: SpockT (WebStateM R.RethinkDBHandle () ()) ()
appRoutes = do
  post "message" insertMessageAction

  get "message" listMessagesAction


insertMessageAction :: ActionT (WebStateM R.RethinkDBHandle () ()) ()
insertMessageAction = do
  message <- jsonBody'
  runQuery $ insertMessage message
  json message

listMessagesAction :: ActionT (WebStateM R.RethinkDBHandle () ()) ()
listMessagesAction = do
  messages <- runQuery $ \h -> findMessagesByRecipient "jb" h
  json messages

wsapp :: WS.ServerApp
wsapp pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  sendMessages conn

sendMessages :: WS.Connection -> IO ()
sendMessages conn =
  bracket connectToRethinkDB R.close
  (\h -> do
      messageCursor <- findMessagesByRecipientChanges  "jb" h
      R.each messageCursor $
        \m -> WS.sendTextData conn (getText m))

main :: IO ()
main = do
  app <- spockAsApp $ spock (defaultSpockCfg () dbConn ())  $ appMiddleware >> appRoutes
  Warp.runSettings
        (Warp.setPort 3000 Warp.defaultSettings)
        (WaiWS.websocketsOr WS.defaultConnectionOptions wsapp app)
