module Monad.BotM where

import Types hiding (Config(..))
import qualified Types as T
import Control.Monad.State
import Control.Concurrent.STM
import Control.Monad.Trans.Reader

import Text.Regex.PCRE ((=~))

import Control.Concurrent
import System.IO

type BotM = State T.Config

behavior :: Behavior () -> BotM ()
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

onMessageMatch regex fn = behavior $ do
  cmd <- asks command
  case cmd of
    PRIVMSG src msg ->
      let matches = msg =~ regex :: [[String]]
      in if null matches
            then return ()
            else fn matches
    _ -> return ()
