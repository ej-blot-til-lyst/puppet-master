{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- import Puppetry.Slave
import Puppetry.WebSocketServer
import Data.Text as T


type State = ()
type Action
  = MsgReceived T.Text

main :: IO ()
main = do
  print "Hallo"
  simpleServer (WebSocketServer update () (MsgReceived) init)

update action state =
  case action of
    MsgReceived message ->
      (state, Broadcast $ T.pack ("Nemlig " ++ (T.unpack message)))

init = ()
