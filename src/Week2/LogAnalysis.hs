module Week2.LogAnalysis ( parseMessage,
                           parse
) where

import Week2.Log

parseMessage :: String -> LogMessage
parseMessage m = case words m of
  "I" : t : s  -> LogMessage Info (read t) (unwords s)
  "W" : t : s -> LogMessage Warning (read t) (unwords s)
  "E" : c : t : s -> LogMessage (Error (read c)) (read t) (unwords s)
  _  -> Unknown m

parse :: String -> [LogMessage]
parse t = map parseMessage $ lines t
