{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Puppetry.Slave

main :: IO ()
main = do
  print "hello"
  serve action

action :: Action
action message =
  message
