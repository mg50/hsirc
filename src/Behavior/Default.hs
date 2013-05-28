
module Behavior.Default where
import Command
import Types
import Control.Monad.Trans.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Concurrent.STM
import Control.Concurrent.MVar

runBehavior :: Behavior a -> Bot -> Command -> IO (Maybe a)
runBehavior m b c = runMaybeT $ runReaderT m $ ResponseEnv b c

pingPong :: Behavior ()
pingPong = do cmd <- asks command
              case cmd of
                PING x -> ircCommand $ PONG x
                _      -> return ()

ircCommand :: Command -> Behavior ()
ircCommand cmd = do b <- asks bot
                    liftIO $ atomically $ writeTChan (writingChannel b) cmd

reply :: String -> Behavior ()
reply msg = do (PRIVMSG src _) <- asks command
               ircCommand $ PRIVMSG src msg

broadcast :: String -> Behavior ()
broadcast msg = do b <- asks bot
                   let chans = ircChans (config b)
                   let cmds = map (\c -> PRIVMSG (Channel c "") msg) chans
                   sequence_ $ map ircCommand cmds

joinChannels :: Behavior ()
joinChannels = do b <- asks bot
                  let chans = ircChans (config b)
                  let cmds = map (\c -> JOIN c) chans
                  sequence_ $ map ircCommand cmds

startup :: Behavior ()
startup = do b <- asks bot
             ircCommand $ NICK $ (nick . config) b
             ircCommand $ USER $ (nick . config) b ++ " 0 * :tutorial bot"
             joinChannels

quit :: Behavior ()
quit = do b <- asks bot
          liftIO $ putMVar (done b) ()
