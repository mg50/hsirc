{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
module Types where

import System.IO
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

type Nick = String
type IRCChannelName = String

data Config = Config { server :: String
                     , port :: Int
                     , nick :: String
                     , password :: String
                     , ircChans :: [String]
                     , behaviors :: [Behavior ()]
                     }


data Bot = Bot { config :: Config
               , handle :: Handle
               , writingChannel :: TChan Command
               , done :: MVar ()
               }

data MessageSource = Channel IRCChannelName Nick | User Nick

data Command = PING String
             | PONG String
             | PRIVMSG MessageSource String
             | NICK String
             | USER String
             | JOIN String

data ResponseEnv = ResponseEnv {bot :: Bot, command :: Command}

type Behavior = ReaderT ResponseEnv (MaybeT IO)
