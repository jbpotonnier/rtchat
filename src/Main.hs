{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Message                              (Message (..),
                                                       findMessagesByRecipient)

import qualified Data.Text.IO                         as TextIO
import qualified Database.RethinkDB                   as R

import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Handler.WebSockets       as WaiWS
import qualified Network.WebSockets                   as WS

import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static
import           Web.Spock

appMiddleware :: SpockT IO ()
appMiddleware = do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "static")

appRoutes :: SpockT IO ()
appRoutes =
  get root $
   text "OK"


wsapp :: WS.ServerApp
wsapp pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  sendMessages conn

sendMessages :: WS.Connection -> IO ()
sendMessages conn = do
  h <- R.connect "localhost" 28015 Nothing
  let database = R.db "test"
  let _ = R.use database h
  messageCursor <- findMessagesByRecipient h "jb"
  R.each messageCursor $
    \ m -> do
      TextIO.putStrLn (getText m)
      WS.sendTextData conn (getText m)
  R.close h

main :: IO ()
main = do
  app <- spockAsApp $ spockT id $ appMiddleware >> appRoutes
  Warp.runSettings
        (Warp.setPort 3000 Warp.defaultSettings)
        (WaiWS.websocketsOr WS.defaultConnectionOptions wsapp app)
