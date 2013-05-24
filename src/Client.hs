module Client where

import Types hiding (Config(..))
import qualified Types as T
import Control.Monad
import Control.Monad.State
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Connection (connect, write)
import Command
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



newConfig :: T.Config
newConfig = T.Config "" 0 "" "" [] []

createBot :: BotM a -> IO Bot
createBot m = let conf = execState m newConfig
              in do chan <- newTChanIO
                    mvar <- newEmptyMVar
                    hdl <- connect conf

                    return $ Bot conf hdl chan mvar


doCommand :: Command -> Bot -> IO ()
doCommand cmd bot = atomically $ writeTChan (writingChannel bot) cmd

ignore :: Bot -> IO ()
ignore _ = return ()

logMsg :: String -> IO ()
logMsg s = putStrLn $ "LOGGING: " ++ s

joinChannels :: Bot -> IO ()
joinChannels bot =
  let chans = T.ircChans $ config bot
  in sequence_ $ map (\c -> doCommand (JOIN c) bot) chans

serve :: BotM a -> IO ()
serve botm =
  do bot <- createBot botm
     write (handle bot) $ NICK $ (T.nick . config) bot
     write (handle bot) $ USER $ (T.nick .config) bot ++ " 0 * :tutorial bot"

     joinChannels bot

     readThreadId <- forkIO $ forever $ do s <- hGetLine (handle bot)
                                           logMsg s
                                           case parseCommand s of
                                             Right cmd ->
                                               forM_ (T.behaviors $ config bot) $ \b -> b cmd bot
                                             _ -> return ()

     writeThreadId <- forkIO $ forever $ do s <- atomically $ readTChan (writingChannel bot)
                                            write (handle bot) s


     takeMVar (done bot)
     killThread readThreadId
     killThread writeThreadId
     return ()
