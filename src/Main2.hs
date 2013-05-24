
module Main where
import Types(Config(..), Bot(..))
import Client
import Control.Concurrent.STM.TChan
import Control.Concurrent.MVar

newConfig :: Config
newConfig = Config "" 0 "" "" []

newBot :: IO Bot
newBot = do chan <- newTChanIO
            mvar <- newEmptyMVar
            return $ Bot newConfig Nothing chan mvar []


defineBot :: BotM () -> IO ()
defineBot m = runStateT m newBot

-- newBot :: IO Bot
-- newBot = do chan <- newEm
--   Bot newConfig Nothing

-- listen = do
--   hdl <- connect

-- listen :: BotM -> IO ()
-- listen bot =
--   do let hdl  = handle bot
--      let conf = config bot
--      sendMessage hdl "NICK" (nick conf)
--      sendMessage hdl "USER" $ (nick conf) ++ " 0 * :tutorial bot"
-- --     write hdl "NICK"
--      readThreadId <- forkIO $ forever $ do s <- hGetLine hdl
--                                            process bot s
--      writeThreadId <- forkIO $ forever $ do s <- atomically $ readTChan (writingChannel bot)
--                                             hPutStrLn hdl s
