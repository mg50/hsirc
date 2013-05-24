
module Connection where

import Types(Config(..))
import System.IO
import Network


connect :: Config -> IO Handle
connect config = do h <- connectTo (server config) $ PortNumber (fromIntegral $ port config)
                    hSetBuffering h NoBuffering
                    return h

write :: (Show a) => Handle -> a -> IO ()
write h msg = do hPutStrLn h msg'
                 putStr $ "SENDING: " ++ msg' ++ "\r\n"
  where msg' = show msg
