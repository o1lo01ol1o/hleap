{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}


module System.Hardware.Leap (
  Configuration(..)
, Handler
, run
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
import Network.WebSockets (Connection, receiveData, runClient, sendTextData)
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


run :: FromJSON a => Configuration -> Handler a -> IO ()
run Configuration{..} handler =
  runClient host port "/v6.json" $ \connection ->
    do
      version' <- eitherDecode <$> receiveData connection
      case version' of
        Right v@Version{} -> print v
        Left  s -> error s
      setFocused connection True
      setGestures connection True
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
  parseJSON _x = empty -- error $ "Failed to parse JSON: " ++ show _x


setBackground :: Connection -> Bool -> IO ()
setBackground = setSomething "background"


setFocused :: Connection -> Bool -> IO ()
setFocused = setSomething "focused"


setGestures :: Connection -> Bool -> IO ()
setGestures = setSomething "gestures"


setOptimizedHMD :: Connection -> Bool -> IO ()
setOptimizedHMD = setSomething "optimizedHMD"


setSomething :: String -> Connection -> Bool -> IO ()
setSomething item connection enabled =
  sendTextData connection
    $ pack
    $ "{\"" ++ item ++ "\" : " ++ (if enabled then "true" else "false") ++ "}"
