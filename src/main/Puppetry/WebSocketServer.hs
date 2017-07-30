{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- The goal of this module is to provide an interface to create
-- a web socket server, using wai.

-- The module follows the structure.

module Puppetry.WebSocketServer
  ( WebSocketServer (..)
  , simpleServer
  , Effect (..)
  ) where

import Prelude hiding (init)

import Control.Exception (finally)
import Control.Concurrent  (MVar, newMVar, modifyMVar, modifyMVar_)
import Control.Monad (forever, forM_)

import qualified Data.List                      as List
import qualified Data.Maybe                     as Maybe
import qualified Data.IntMap.Strict             as IntMap
import qualified Data.Text                      as Text
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Safe


type ClientId = Int

type ClientMap = IntMap.IntMap WS.Connection

type Msg = Text.Text

data WebSocketServer action state =
  WebSocketServer
  { update :: action -> state -> (state, Effect)
  , subscription :: Subscription action
  , decodeMsg :: Msg -> action
  , init :: state
  }

type SocketState state = (state, ClientMap)

startServer :: WebSocketServer a s -> IO WS.ServerApp
startServer options = do
  state <- newMVar (init options, IntMap.empty)
  return $ acceptConnection state

  where
    acceptConnection stateRef pendingConn = do
      -- accept the incoming connection
      conn <- WS.acceptRequest pendingConn

      clientId <- registerClient stateRef conn

      -- Keep connection alive, by pinging every 30 second
      WS.forkPingThread conn 30

      -- Listen until the connection is lost
      finally (listen stateRef conn) (disconnectClient stateRef clientId)

    listen stateRef conn = do
      action <- decodeMsg options <$> WS.receiveData conn
      updateServer stateRef action

    updateServer stateRef action = do
      modifyMVar_ stateRef $ \(s, cm) -> do
        let (s', e) = update options action s
        handleEffect e cm
        return (s', cm)

simpleServer :: WebSocketServer a s -> IO ()
simpleServer options = do
  server <- startServer options
  Warp.run 3000 $ WS.websocketsOr
    WS.defaultConnectionOptions
    server
    httpApp

httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"

-- Handlers

registerClient :: MVar (SocketState s) -> WS.Connection -> IO ClientId
registerClient stateRef conn =
  modifyMVar stateRef $ \(s, cm) -> do
     let clientId = Safe.maximumDef (-1) (IntMap.keys cm) + 1
     return ((s, IntMap.insert clientId conn cm), clientId)

disconnectClient :: MVar (SocketState s) -> ClientId -> IO ()
disconnectClient stateRef clientId =
  modifyMVar_ stateRef $ \(s, cm) -> do
    return (s, IntMap.delete clientId cm)

getClient :: ClientMap -> ClientId -> Maybe.Maybe WS.Connection
getClient cm cid = IntMap.lookup cid cm

getClients :: ClientMap -> [WS.Connection]
getClients cm = IntMap.elems cm

-- Subscriptions

type Subscription action = ()

-- Effect

data Effect
  = SendMessage ClientId Text.Text
  | Broadcast Text.Text
  | EBatch [Effect]
  | ENoOp

handleEffect :: Effect -> ClientMap -> IO ()
handleEffect e cm =
  case e of
    SendMessage cid txt ->
      case getClient cm cid of
        Maybe.Just conn -> WS.sendTextData conn txt
        otherwise -> undefined
    Broadcast txt ->
      forM_ (getClients cm) $ flip WS.sendTextData txt
    EBatch lst ->
      forM_ lst (flip handleEffect cm)
    ENoOp ->
      return ()
