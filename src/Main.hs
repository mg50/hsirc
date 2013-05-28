
module Main where

import Client
import Monad.BotM
import Behavior.Default

myBot = do server "irc.freenode.net"
           port 6667
           ircChans ["hellohello"]
           nick "thebot"

           onMessageMatch "^!bot repeat (.*)" $ \matches ->
             reply $ "you said: " ++ (matches !! 0 !! 1)

           onMessageMatch "^!bot quit" $ const quit


main = serve myBot
