module Week2.LogAnalysis ( parseMessage,
                           parse,
                           insert,
                           build,
                           inOrder,
                           whatWentWrong
) where

import Week2.Log

parseMessage :: String -> LogMessage
parseMessage m = case words m of
  "I" : t : s     -> LogMessage Info (read t) (unwords s)
  "W" : t : s     -> LogMessage Warning (read t) (unwords s)
  "E" : c : t : s -> LogMessage (Error (read c)) (read t) (unwords s)
  _  -> Unknown m

parse :: String -> [LogMessage]
parse t = map parseMessage $ lines t

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m@(LogMessage _ ts _) tree = case tree of
  Leaf                                           -> Node Leaf m Leaf
  Node left nodeM@(LogMessage _ nodeTs _ ) right -> if   ts < nodeTs
                                                    then Node (insert m left) nodeM right 
                                                    else Node left nodeM $ insert m right

build :: [LogMessage] -> MessageTree
build [] = Leaf
build l = foldr insert Leaf l

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                = []
inOrder (Node left m right) = inOrder left ++ [m] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong l = map show $ map getMessage $ filter isSevere $ inOrder $ build l
  where isSevere (LogMessage (Error c ) _ _ ) = c > 50 
        isSevere _                            = False
        getMessage (LogMessage _ _ m) = m
