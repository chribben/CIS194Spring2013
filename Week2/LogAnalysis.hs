{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
    import Log
    parseMessage :: String -> LogMessage
    parseMessage message = 
        let logLineWords = words message
        in
            case logLineWords of
                "E":severity:timestamp:msg -> LogMessage (Error $ read severity) (read timestamp) (unwords msg)
                "I":timestamp:msg -> LogMessage Info (read timestamp) (unwords msg)
                "W":timestamp:msg -> LogMessage Warning (read timestamp) (unwords msg)
                _ -> Unknown message

    parse :: String -> [LogMessage]
    parse file = 
        let rows = lines file
        in map (\r -> parseMessage r) rows

    insert :: LogMessage -> MessageTree -> MessageTree
    insert (Unknown _) leaf@(Leaf) = leaf
    insert lm@(LogMessage _ _ _) Leaf = Node Leaf lm Leaf
    insert (Unknown _) tree = tree
    insert lm@(LogMessage _ ts1 _) (Node left lm2@(LogMessage _ ts2 _) right) = 
        if ts1 > ts2 then 
            Node left lm2 (insert lm right)
        else
            Node (insert lm left) lm2 right

    build :: [LogMessage] -> MessageTree
    build lms = let addLogMessagesToTree [] tree = tree
                    addLogMessagesToTree (h:t) tree = addLogMessagesToTree t (insert h tree)
                in
                   addLogMessagesToTree lms Leaf

    inOrder :: MessageTree -> [LogMessage]
    inOrder Leaf = []
    inOrder (Node left lm right) = (inOrder left) ++ (lm:(inOrder right)) 

    whatWentWrong :: [LogMessage] -> [String]
    whatWentWrong lms = let ordered = inOrder $ build lms
                        in map (\lm -> let (LogMessage _ _ msg) = lm in msg) $ 
                             filter 
                                (\lm -> case lm of
                                    (LogMessage (Error sev) _ _) -> sev >= 50
                                    _ -> False)
                                ordered