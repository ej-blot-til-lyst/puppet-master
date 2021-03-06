{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Example (runApp, app) where

import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)
import qualified Web.Scotty as S

import           Puppetry.Protocol

app' :: S.ScottyM ()
app' = do
  S.get "/" $ do
    S.text "hello"

  S.get "/some-json" $ do
    S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = do
  putStrLn "Hello, world! 3"

  runPuppetry "fp" defaultPuppetrySettings $ do
    set (everything { arrays = noArrays { frontlight = True }}) cRed
    set (everything { arrays = noArrays { backlight = True }}) cBlue

  S.scotty 8080 app'
