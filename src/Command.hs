module Command (show, parseCommand) where

import Types (Command(..), MessageSource(..))
import Text.ParserCombinators.Parsec

instance Show Command where
  show (JOIN chan) = if head chan == '#'
                        then "JOIN " ++ chan
                        else "JOIN " ++ "#" ++ chan
  show (PING s) = "PING :" ++ s
  show (PONG s) = "PONG :" ++ s
  show (NICK s) = "NICK " ++ s
  show (USER s) = "USER " ++ s ++ " 0 * :tutorial bot"
  show (PRIVMSG source msg) = case source of
                                Channel s -> "PRIVMSG " ++ s ++ " #" ++ msg
                                User    s -> "PRIVMSG " ++ s ++ " " ++ msg

privmsg :: Parser Command
privmsg = do string "PRIVMSG "
             c <- anyChar
             cs <- many alphaNum
             space
             msg <- many anyChar

             return $ if c == '#'
                         then PRIVMSG (Channel cs) msg
                         else PRIVMSG (User (c:cs)) msg

nick :: Parser Command
nick = do string "NICK "
          cs <- many anyChar
          return $ NICK cs

ping :: Parser Command
ping = do string "PING :"
          cs <- many anyChar
          return $ PING cs

pong :: Parser Command
pong = do string "PONG :"
          cs <- many anyChar
          return $ PONG cs

command = try privmsg <|> try nick <|> try ping <|> try pong

parseCommand cmd = parse command "" cmd
