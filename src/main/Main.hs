{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Puppetry.Slave
import Data.Text as T

main :: IO ()
main = do
  print "Hallo"
  serve action

action :: Action
action message =
  T.pack $ "Nemlig " ++ (T.unpack message)
