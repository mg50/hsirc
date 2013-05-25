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
  show (PRIVMSG source msg)
    = case source of
        Channel chan nick -> "PRIVMSG #" ++ chan ++ " " ++ msg
        User nick         -> "PRIVMSG " ++ nick ++ " " ++ msg


userPrivmsg :: Parser Command
userPrivmsg = do char ':'
                 nick <- many $ noneOf "!"
                 char '!'
                 many $ noneOf " "
                 spaces
                 string "PRIVMSG"
                 spaces
                 many $ noneOf "# "
                 spaces
                 char ':'
                 msg <- many anyChar
                 return $ PRIVMSG (User nick) msg

chanPrivmsg :: Parser Command
chanPrivmsg = do char ':'
                 nick <- many $ noneOf "!"
                 char '!'
                 many $ noneOf " "
                 spaces
                 string "PRIVMSG"
                 spaces
                 char '#'
                 chan <- many $ noneOf " "
                 spaces
                 char ':'
                 msg <- many anyChar
                 return $ PRIVMSG (Channel chan nick) msg


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

command = try chanPrivmsg <|> try userPrivmsg <|> try nick <|> try ping <|> try pong

parseCommand cmd = parse command "" cmd
