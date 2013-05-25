
module Main where

import Client
import Monad.BotM
import Types(Command(..))

bot :: BotM ()
bot = do server "irc.freenode.net"
         port 6667
         ircChans ["hellohello"]
         nick "thebot"

         behavior $ \cmd bot -> case cmd of
                                  PING x -> doCommand (PONG x) bot
                                  _      -> return ()

         behavior $ \cmd bot -> case cmd of
                                  m@(PRIVMSG src msg) -> doCommand m bot
                                  _                   -> return ()


main = serve bot
