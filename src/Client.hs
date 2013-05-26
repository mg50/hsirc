module Client where

import Types hiding (Config(..))
import qualified Types as T
import Control.Monad.State
import Control.Concurrent.STM
import Connection (connect, write)
import Command
import Control.Concurrent
import System.IO
import Monad.BotM
import Behavior.Default (pingPong)

newConfig :: T.Config
newConfig = T.Config "" 0 "" "" [] [pingPong]

createBot :: BotM a -> IO Bot
createBot m = let conf = execState m newConfig
              in do chan <- newTChanIO
                    mvar <- newEmptyMVar
                    hdl <- connect conf

                    return $ Bot conf hdl chan mvar

logMsg :: String -> IO ()
logMsg s = putStrLn $ "LOGGING: " ++ s

joinChannels :: Bot -> IO ()
joinChannels bot =
  let chans = T.ircChans $ config bot
  in sequence_ $ map (\c -> doCommand (JOIN c) bot) chans



readLoop :: Bot -> IO ThreadId
readLoop bot =
  forkIO $ forever $ do s <- hGetLine (handle bot)
                        logMsg s
                        case parseCommand s of
                          Right cmd ->
                            forM_ (T.behaviors $ config bot) $ \behavior ->
                              behavior cmd bot
                          _ -> return ()

writeLoop :: Bot -> IO ThreadId
writeLoop bot =
  forkIO $ forever $ do s <- atomically $ readTChan (writingChannel bot)
                        write (handle bot) s

say bot src msg = doCommand (PRIVMSG src msg) bot

serve :: BotM a -> IO ()
serve botm =
  do bot <- createBot botm
     write (handle bot) $ NICK $ (T.nick . config) bot
     write (handle bot) $ USER $ (T.nick .config) bot ++ " 0 * :tutorial bot"

     joinChannels bot

     readThreadId <- readLoop bot
     writeThreadId <- writeLoop bot


     takeMVar (done bot)
     killThread readThreadId
     killThread writeThreadId
     return ()
