module Core.View where
    import Data.List
    import Core.Types
    import Data.Char 
    import Control.Monad.State as State
    import System.Console.ANSI
    import Text.Read as Read
    import Core.Util

  
   -- | ###########################################################################################
   -- | ############################## Get user input from the terminal ###########################
   -- | ###########################################################################################

    -- | Get the answer about if the player want an other trie
    getAUTrie :: IO String   
    getAUTrie = do
        reply <- getLine
        case reply of
                "Y" -> return reply
                "N" -> return reply
                _   -> putStrLn ("\nYour input is invalid, it should be 'Y' or 'N'. Try again:") >> getAUTrie

    -- | Get the number of token from the user
    getINbToken :: IO Int
    getINbToken = do
        putStrLn $ "\nEnter the number of token:"
        nb <- getLine 
        case readMaybe nb of
            Just a  -> returnTokenNb a
            Nothing -> do 
                putStrLn ("\nYour input is invalid, it should be a number. Try again.") >> getINbToken

    -- | Function to handle if the user add a number of token less than zero 
    returnTokenNb :: Int -> IO Int
    returnTokenNb nb 
        | nb <= 0 = do 
            putStrLn $ "\nThe number of token should be greater than 0, try again:" 
            getINbToken
        | otherwise = return nb
       
    -- Get the number of player
    getINbPlayers :: IO Int
    getINbPlayers = do
        putStrLn $ "\nEnter the number of players:" 
        nb <- getLine 
        case Read.readMaybe nb of
            Just a  -> returnNbPlayer a
            Nothing -> putStrLn ("\nYour input is invalid ( " ++ nb ++ " ), it should be a number. Try again.") >> getINbPlayers

    -- | Function to handle if the user add a number of player less than two
    returnNbPlayer :: Int -> IO Int
    returnNbPlayer nb 
        | nb < 2 = do
            putStrLn $ "\nThe number of players should be greater than 1, try again."
            getINbPlayers
        | otherwise = return nb
        
    -- | Get the name of a player
    getIPName :: PID -> IO String
    getIPName pID = do
        putStrLn $ concat ["\nPlayer ", show pID, ", enter your name:"]
        name <- getLine
        case name of 
            (x:xs)  -> return (toUpper x : xs)  
            _ -> do 
                putStrLn ("\nYour input is empty, you have to give a name or surname or pseudo. Try again:") >> getIPName pID
       
    -- | Function to get the name of several player
    getIPNames :: Int -> IO [String]
    getIPNames playerNb = do
        pNames <- sequence [getIPName nb | nb <- [1..playerNb]]
        return pNames


   -- | ###########################################################################################
   -- | ############################## Print message on the terminal ##############################
   -- | ###########################################################################################

    printR0Winner :: PName -> IO ()
    printR0Winner name = do 
        putStrLn $ "Congrats "
                ++ name
                ++ ", you have the highest score.\nYou will start to rolls the dice first in the round 1.\n\n"
                ++ name ++ ", when you are ready 'press' enter to start the first round."


    -- Function to print the round winner
    printRWinner :: StateT GameStates IO () 
    printRWinner = do
        gs <- State.get

        let round = _round gs
        let (_,rWinner,_) = _rWinner gs 
        printScoreboard
        State.lift . putStrLn $ "Congrats " 
                ++ rWinner 
                ++ ", you have win the round "
                ++ show (round) ++ ".\n"
                ++ "You will play first in the round "
                ++ show (round + 1) ++ "."
                ++ "\n\n"
                ++ rWinner ++ ", press 'Enter' when you are ready to start the next round."
                ++ "\n\n"
        _ <- State.lift $ getLine
        State.lift $ clearScreen
        return ()
        

    -- | Function to print a message if the player have a better score than the round leader
    printNewLMsg :: StateT GameStates IO ()
    printNewLMsg = do
        gs <- State.get
        let id = fst $ _rLeader gs 
        let scoreboard = _scoreboard gs 
        let players = _players gs
        let pName = getPName id $ _players gs  
        print scoreboard players pName   
        where 
            -- Function to print only the message if the player is not the first or the last to play
            print scb ps name 
                | length scb == 1 || length scb == length ps = do State.lift $ return ()  
                | otherwise = State.lift . putStrLn $ "Congrat's "
                                                      ++ name   
                                                      ++ ", your are the new leader.\n" 

        

    -- | Function to print the number of the current round
    printRound :: StateT GameStates IO ()
    printRound = do
        gs <- State.get
        State.lift . putStrLn $ concat ["\n",
                                replicate 30 '-',
                                " ",
                                "Round ",
                                show $ _round gs,
                                " ",
                                replicate 30 '-',
                                "\n"]

    -- | Function to print the line of the round 0 
    printR0 :: IO ()
    printR0 = do
        putStrLn $ concat ["\n",
                        replicate 30 '-',
                        " ",
                        "Round ",
                        show 0,
                        " ",
                        replicate 30 '-',
                        "\n"]

    -- | Print the name of the game winner and 
    printGWinner :: StateT GameStates IO ()
    printGWinner = do
        gs <- State.get
        let winnerName = getName $ getGWinner gs 
        State.lift . putStrLn $ concat ["Congrat's ", winnerName ," you win the game."]

    -- | Function to print a line                         
    printLine :: StateT GameStates IO () 
    printLine = do State.lift . putStrLn $ "\n" ++ replicate 68 '-' ++ "\n"
    
                
    -- | Function to print how many tries is left
    printTrieMsg :: PName -> Tries -> StateT GameStates IO ()
    printTrieMsg pName t = do
        gs <- State.get
        State.lift . putStrLn $ concat [pName,
                                ", you have ",
                                show t,
                                " tries",
                                if t == 1 then " left.\n" else ".\n",
                                "Press 'Enter' to roll the dice..."]

  
    {-- 
    Function to draw a grid representing three dice:
    +-------+  +-------+  +-------+ 
    | a b c |  | a b c |  | a b c |
    | d e f |  | d e f |  | d e f |
    | g h i |  | g h i |  | g h i |
    +-------+  +-------+  +-------+
    --}
    diceGrid :: [String] -> String
    diceGrid [dNb1,dNb2,dNb3] = concat $ 
                                [   
                                    "+", replicate 7 '-', "+  ",
                                    "+", replicate 7 '-', "+  ",
                                    "+", replicate 7 '-', "+  ", "\n",
                                    replaceGridElem dNb1 $ concat ["| ", intersperse ' ' ['a'..'c'], " |  "],
                                    replaceGridElem dNb2 $ concat ["| ", intersperse ' ' ['a'..'c'], " |  "],
                                    replaceGridElem dNb3 $ concat ["| ", intersperse ' ' ['a'..'c'], " |  ", "\n"],
                                    replaceGridElem dNb1 $ concat ["| ", intersperse ' ' ['d'..'f'], " |  "],
                                    replaceGridElem dNb2 $ concat ["| ", intersperse ' ' ['d'..'f'], " |  "],
                                    replaceGridElem dNb3 $ concat ["| ", intersperse ' ' ['d'..'f'], " |  ", "\n"],
                                    replaceGridElem dNb1 $ concat ["| ", intersperse ' ' ['g'..'i'], " |  "],
                                    replaceGridElem dNb2 $ concat ["| ", intersperse ' ' ['g'..'i'], " |  "],
                                    replaceGridElem dNb3 $ concat ["| ", intersperse ' ' ['g'..'i'], " |  ", "\n"],
                                    "+", replicate 7 '-', "+  ",
                                    "+", replicate 7 '-', "+  ",
                                    "+", replicate 7 '-', "+  ", "\n"
                                ]


    -- Function to replace the element of the dice grid.
    replaceGridElem :: [Char] -> [Char] -> [Char]
    replaceGridElem = \nb grid -> [replace l nb | l <- grid]
            where 
                replace e dGrid  
                    | e `elem` dGrid = '*'
                    | otherwise = 
                        case (elem e ['a'..'i']) of
                            True  -> ' ' 
                            False -> e

    -- | Function to draw each face dice corresponding to the rolls number
    drawTheDice :: Rolls -> IO ()
    drawTheDice (Rolls l) = do
        putStrLn (diceGrid [lToReplace diceNb | diceNb <-l])
        where
            -- Replace the number by the letters corresponding the face to draw 
            lToReplace nb = 
                case nb of 
                    One -> "e"      -- Side with the number 1
                    Two -> "ai"     -- Side with the number 2
                    Three -> "aei"  -- Side with the number 3
                    Four -> "acgi"  -- Side with the number 4
                    Five -> "acegi" -- Side with the number 5
                    Six -> "adgcfi" -- Side with the number 6

        
    -- Function to print the player score in the scoreboard
    printPScore :: Players -> GameStates -> IO ()
    printPScore [] _ = return ()
    printPScore (x:xs) gs = do
        let (pid, name, token) = x
        let score = formatScore $ getRollsbyId pid (_scoreboard gs)
        let spaceName = 15 - (length name) 
        let spaceToken = 7 - (length . show $ token)
        let spaceRound = 28 - (length score)
        putStrLn $ concat ["| " ++ show pid ++ "  | ",
                    name,
                    replicate spaceName ' ' ++ "| ",
                    show token,
                    replicate spaceToken ' ' ++ "| ",
                    score, 
                    if isLeader x (_rLeader gs) then " (L)" else "    ",
                    replicate spaceRound ' ' ++ "|"]
        printPScore xs gs


    -- Function to print the scoreboard
    printScoreboard :: StateT GameStates IO ()
    printScoreboard = do
        gs <- State.get
        let space =  26 - (length . show $ _round gs)
        State.lift . putStrLn $ concat ["+", replicate 64 '-', "+"]
        State.lift . putStrLn $ concat ["| ID | Player Name    | Token  | Round ", show $ _round gs, replicate space ' ', "| " ]
        State.lift . putStrLn $ concat ["+", replicate 64 '-', "+"]
        State.lift $ printPScore (_players gs) gs 
        State.lift . putStrLn $ concat ["+", replicate 64 '-', "+\n"]
   