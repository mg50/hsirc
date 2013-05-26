module Monad.BotM where

import Types hiding (Config(..))
import qualified Types as T
import Control.Monad.State
import Control.Concurrent.STM


import Control.Concurrent
import System.IO

type BotM = State T.Config

behavior :: Behavior -> BotM ()
behavior b = modify $ \conf -> let bs = T.behaviors conf
                               in conf{T.behaviors = (b:bs)}


server :: String -> BotM ()
server s = modify $ \conf -> conf{T.server = s}

port :: Int -> BotM ()
port s = modify $ \conf -> conf{T.port = s}

nick :: String -> BotM ()
nick s = modify $ \conf -> conf{T.nick = s}

ircChans :: [String] -> BotM ()
ircChans s = modify $ \conf -> conf{T.ircChans = s}

doCommand :: Command -> Bot -> IO ()
doCommand cmd bot = atomically $ writeTChan (writingChannel bot) cmd

onChanMessage :: (IRCChannelName -> Nick -> String -> Bot -> IO ()) -> BotM ()
onChanMessage fn = behavior $ \cmd bot ->
  case cmd of
    PRIVMSG (Channel chan nick) msg -> fn chan nick msg bot
    _ -> return ()

onUserMessage :: (Nick -> String -> Bot -> IO ()) -> BotM ()
onUserMessage fn = behavior $ \cmd bot ->
  case cmd of
    PRIVMSG (User nick) msg -> fn nick msg bot
    _ -> return ()
