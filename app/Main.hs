{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


-- import Puppetry.Slave
import Puppetry.WebSocketServer hiding (update)
import Data.Text as T


type State = ()
data Action
  = MsgReceived T.Text

main :: IO ()
main = do
  print "Hallo"
  simpleServer $
    WebSocketServer update () (MsgReceived) ()

update action state =
  case action of
    MsgReceived message ->
      (state, Broadcast $ T.pack ("Nemlig " ++ (T.unpack message)))

