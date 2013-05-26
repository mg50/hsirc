
module Behavior.Default where
import Command
import Types
import Monad.BotM (doCommand)

pingPong :: Behavior
pingPong cmd bot = case cmd of
                     PING x -> doCommand (PONG x) bot
                     _      -> return ()
