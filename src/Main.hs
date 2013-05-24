
module Main where

import Client
import Types(Command(..))

bot :: BotM ()
bot = do server "irc.freenode.net"
         port 6667
         ircChans ["hellohello"]
         nick "thebot"

         behavior $ \cmd -> case cmd of
                              PING x -> doCommand $ PONG x
                              _      -> ignore

         behavior $ \cmd -> case cmd of
                              m@(PRIVMSG src msg) -> doCommand m
                              _                   -> ignore

main = serve bot
