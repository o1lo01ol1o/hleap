{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}


module System.Hardware.Leap (
  Configuration(..)
, ClientApp
, Handler
, run
, runWithHandler
, Version(..)
, setBackground
, setFocused
, setGestures
, setOptimizedHMD
) where


import Control.Applicative (empty)
import Control.Monad (forever)
import Data.Aeson (FromJSON(..), Value(..), (.:), eitherDecode)
import Data.Default (Default(..))
import Data.Text (pack)
import Network.WebSockets (ClientApp, Connection, receiveData, runClient, sendTextData)
import System.Hardware.Leap.Event (Event(..))


data Configuration =
  Configuration
  {
    host :: String
  , port :: Int
  }
    deriving (Eq, Ord, Read, Show)

instance Default Configuration where
  def =
    Configuration
    {
      host = "localhost"
    , port = 6437
    }


type Handler a = Event a -> IO ()


run :: Configuration -> ClientApp a -> IO a
run Configuration{..} app =
  runClient host port "/v6.json" $ \connection ->
    do
      version' <- eitherDecode <$> receiveData connection
      case version' of
        Right v@(Version _ 6) -> putStrLn $ "Leap connection" ++ show v
        Right v               -> error $ "Incorrect version: " ++ show v
        Left  s               -> error s
      app connection


type ConnectionModifier = Connection -> IO ()


runWithHandler :: FromJSON a => Configuration -> [ConnectionModifier] -> Handler a -> IO ()
runWithHandler configuration modifiers handler =
  run configuration $ \connection ->
    do
      mapM_ ($ connection) modifiers
      forever $ do
        event' <- eitherDecode <$> receiveData connection
        case event' of
          Right e -> handler e
          Left  s -> error s


data Version =
  Version
  {
    serviceVersion :: String
  , version        :: Int
  }
    deriving (Eq, Ord, Read, Show)

instance FromJSON Version where
  parseJSON (Object o) =
    Version
      <$> o .: "serviceVersion"
      <*> o .: "version"
  parseJSON _ = empty


setBackground :: Bool -> ConnectionModifier
setBackground = setSomething "background"


setFocused :: Bool -> ConnectionModifier
setFocused = setSomething "focused"


setGestures :: Bool -> ConnectionModifier
setGestures = setSomething "enableGestures"


setOptimizedHMD :: Bool -> ConnectionModifier
setOptimizedHMD = setSomething "optimizeHMD"


setSomething :: String -> Bool -> ConnectionModifier
setSomething item enabled connection =
  sendTextData connection
    $ pack
    $ "{\"" ++ item ++ "\" : " ++ (if enabled then "true" else "false") ++ "}"
