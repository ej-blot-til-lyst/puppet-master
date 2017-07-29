-- The goal of this module is to provide an interface to create
-- a web socket server, using wai.

-- The module follows the structure.

module Puppetry.WebSocketServer
  ( WebSocketServer (..)
  , simpleServer
  , Effect (..)
  ) where

import Control.Exception (finally)
import Control.Concurrent  (MVar, newMVar, modifyMVar, modifyMVar_)
import Control.Monad (forever)

import qualified Data.List                      as List
import qualified Data.Maybe                     as Maybe
import qualified Data.IntMap.Strict             as IntMap
import qualified Data.Text                      as Text
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

type WSS a s r :: WebSocketServer a s -> r

startServer :: WebSocketServer a s -> IO Ws.ServerApp
startServer options = do
  state <- Concurrent.newMVar (init options, [])
  return $ acceptConnection state

  where
    acceptConnection state pendingConn = do
      -- accept the incoming connection
      conn <- WS.acceptRequest pendingConn

      clientId <- registerClient state conn

      -- Keep connection alive, by pinging every 30 second
      Ws.forkPingThread conn 30

      -- Listen until the connection is lost
      finally (listen stateRef conn) (disconnectClient state clientId)

    listen stateRef conn =
      action <- decodeMsg options <$> WS.receiveData conn
      updateServer stateRef action

    updateServer stateRef action = do
      modifyMVar stateRef $ \(s, cm) -> do
        let (s', e) = update options action s
        handleEffect e cm

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
     let clientId = Safe.maximumDef -1 (IntMap.keys cm) + 1
     return ((s, IntMap.insert clientId con cm), clientId)

disconnectClient :: MVar (SocketState s) -> ClientId -> IO ()
disconnectClient stateRef stateRef =
  modifyMVar_ stateRef $ \(s, cm) -> do
    return (s, IntMap.delete clientId cm)

getClient :: ClientMap -> ClientId -> Maybe.Maybe WS.Connection
getClient cm cid = IntMap.lookup cm cid

getClients :: ClientMap -> [WS.Connection]
getClients cm = IntMap.values cm

-- Subscriptions

type Subscription action = ()

-- Effect

data Effect
  = SendMessage ClientId Text.Text
  | Broadcast Text.Text
  | Batch [Effect]
  | NoOp

handleEffect :: Effect -> ClientMap -> IO ()
handleEffect e cm =
  case e of
    SendMessage cid txt ->
      case getClient cid cm of
        Maybe.Just conn -> WS.sendTextData conn txt
        otherwise -> undefined
    Broadcast txt ->
      forM_ (getClients cm) $ flip WS.sendTextData txt
    Batch lst ->
      forM_ lst (flip handleEffect cm)
    NoOp ->
    return ()
