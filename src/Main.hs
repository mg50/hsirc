
module Main where

import Client
import Monad.BotM
import Types(Command(..))

bot :: BotM ()
bot = do server "irc.freenode.net"
         port 6667
         ircChans ["hellohello"]
         nick "thebot"

         onMessageMatch "^!bot repeat (.*)" $ \matches src bot ->
           say bot src $ "you said: " ++ (matches !! 0 !! 1)


main = serve bot
