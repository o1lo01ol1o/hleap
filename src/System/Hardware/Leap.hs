{-|
Module      :  $Header$
Copyright   :  (c) 2016-19 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <code@functionally.io>
Stability   :  Stable
Portability :  Portable

Interaction with Leap Motion \<<https://www.leapmotion.com/product/desktop>\>, based on the Web Socket API \<<https://developer.leapmotion.com/documentation/javascript/supplements/Leap_JSON.html>\>.

Here is a simple example applicaton that prints all events:

@
main :: IO ()
main =
  runWithHandler def [setFocused True, setGestures True] $ \event ->
    print (event :: Event Float)
@
-}


{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}


module System.Hardware.Leap (
-- * Configuration
  Configuration(..)
, Version(..)
, setBackground
, setFocused
, setGestures
, setOptimizedHMD
-- * Applications
, ClientApp
, Handler
, run
, runWithHandler
) where


import Control.Applicative (empty)
import Control.Monad (forever)
import Data.Aeson (FromJSON(..), Value(..), (.:), eitherDecode)
import Data.Default (Default(..))
import Data.Text (pack)
import Network.WebSockets (Connection, receiveData, runClient, sendTextData)
import System.Hardware.Leap.Event (Event(..))


-- | Configuration for a connection to a Leap Motion controller via Web Sockets.
data Configuration =
  Configuration
  {
    host :: String -- ^ Host name.
  , port :: Int    -- ^ Port number.
  }
    deriving (Eq, Ord, Read, Show)

instance Default Configuration where
  def =
    Configuration
    {
      host = "localhost"
    , port = 6437
    }


-- | A client application for Leap Motion using Web Sockets.
type ClientApp a =  Connection -- ^ The Web Socet connection.
                 -> IO a       -- ^ Action for the client application.


-- | A handler for Leap Motion events.
type Handler a =  Event a -- ^ The event.
               -> IO ()   -- ^ The action for handling the event.


-- | Run a Leap Motion application using Web Sockets.
run :: Configuration -- ^ The Web Socket configuration.
    -> ClientApp a   -- ^ The client application.
    -> IO a          -- ^ Action for running the client.
run Configuration{..} app =
  runClient host port "/v6.json" $ \connection ->
    do
      version' <- eitherDecode <$> receiveData connection
      case version' of
        Right v@(Version _ 6) -> putStrLn $ "Leap connection" ++ show v
        Right v               -> error $ "Incorrect version: " ++ show v
        Left  s               -> error s
      app connection


-- | Modify a Leap Motion connection.
type ConnectionModifier =  Connection -- ^ The Web Socket connection.
                        -> IO ()      -- ^ Action for modifying the connection.


-- | Run and process events from Leap Motion using Web Sockets.
runWithHandler :: FromJSON a
               => Configuration        -- ^ The Web Socket configuration.
               -> [ConnectionModifier] -- ^ Modifications to be made to the connection.
               -> Handler a            -- ^ The event handler.
               -> IO ()                -- ^ Action for running with event handling.
runWithHandler configuration modifiers handler =
  run configuration $ \connection ->
    do
      mapM_ ($ connection) modifiers
      forever $ do
        event' <- eitherDecode <$> receiveData connection
        case event' of
          Right e -> handler e
          Left  s -> error s


-- | Version information for Leap Motion.
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


-- | Enable or disable background events.  See \<<https://developer.leapmotion.com/documentation/javascript/supplements/Leap_JSON.html>\> for details.
setBackground :: Bool               -- ^ Whether to enable background events.
              -> ConnectionModifier -- ^ Function for making the modification.
setBackground = setSomething "background"


-- | Enable or disable focus.  See \<<https://developer.leapmotion.com/documentation/javascript/supplements/Leap_JSON.html>\> for details.
setFocused :: Bool               -- ^ Whether to enable focus.
           -> ConnectionModifier -- ^ Function for making the modification.
setFocused = setSomething "focused"


-- | Enable or disable gestures.  See \<<https://developer.leapmotion.com/documentation/javascript/supplements/Leap_JSON.html>\> for details.
setGestures :: Bool               -- ^ Whether to enable gestiure events.
            -> ConnectionModifier -- ^ Function for making the modification.
setGestures = setSomething "enableGestures"


-- | Enable or disable head-mounted-display optimization.  See \<<https://developer.leapmotion.com/documentation/javascript/supplements/Leap_JSON.html>\> for details.
setOptimizedHMD :: Bool               -- ^ Whether to enable HMD optimization.
                -> ConnectionModifier -- ^ Function for making the modification.
setOptimizedHMD = setSomething "optimizeHMD"


-- | Enable or disable something in Leap Motion.
setSomething :: String             -- ^ What to enable or disable.
             -> Bool               -- ^ Whether to enable it.
             -> ConnectionModifier -- ^ Function for making the modification.
setSomething item enabled connection =
  sendTextData connection
    $ pack
    $ "{\"" ++ item ++ "\" : " ++ (if enabled then "true" else "false") ++ "}"
