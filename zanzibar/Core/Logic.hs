module Core.Logic where
    import Core.Config
    import Core.Types
    import Core.Util
    import Core.View
    import Data.Either
    import Text.Read as Read
    import Data.List (sort, sortBy)
    import Control.Monad.State as State
    import System.Console.ANSI ( clearScreen )


    -- Function to simulate three rolls dice
    rollThreeDice :: IO Rolls
    rollThreeDice = do
        list <- genNRandomNb 3
        let [nb1, nb2, nb3] = [intToDiceNb nb | nb <- list]
        return $ Rolls [nb1, nb2, nb3]
    
    -- | Rolls the dice in the round 0 
    rollsDice :: Player -> IO (Int, Rolls)
    rollsDice (id, name, _) = do
        printR0
        putStrLn $ name ++ ", press 'Enter' to rolls the dice..." 
        _ <- getLine
        rolls <- rollThreeDice
        drawTheDice rolls 
        return (id, rolls)


    -- | Rolls the dice the winner of the last round or round 0
    leaderRolls :: StateT GameStates IO ()
    leaderRolls = do
        gs <- State.get
        playerRolls (_rWinner gs) defTries
        
    -- | Rolls the dice the players who haven't scored
    otherPRolls :: StateT GameStates IO ()
    otherPRolls  = do
        gs          <- State.get
        let player  = pickPToPlay gs 
        let trie    = _nbOfTry gs 
        case isLeft player of
            False -> return ()
            True -> do
                playerRolls (left player) trie
                otherPRolls
    
    
    -- | Give the players choices to end the game or restart a game
    endGame :: StateT GameStates IO ()
    endGame = do
        -- Print a message with different option for the user
        State.lift $ putStrLn $ "\nDo wou want to play an other game ? :\n"
                                ++ "    - 1 to restart the game with new players.\n"
                                ++ "    - 2 to keep the same players.\n"
                                ++ "    - 3 to stop the game"
    
        -- Get the input user and do the action based of the user chosen option
        inputUser <- State.lift getLine
        case Read.readMaybe inputUser :: Maybe Int of
            Just 1 -> State.lift playGame
            Just 2 -> restartGame
            Just 3 -> do
                State.lift $ putStrLn "\nThe game is over"
                State.lift $ return () 
            Just _ -> do 
                State.lift . putStrLn $ "\nThis option "
                                        ++ show inputUser
                                        ++ " doesn't exist.\n The availabe options are 1 or 2 or 3" 
                endGame
            Nothing -> do 
                State.lift . putStrLn $ "Your input" 
                                        ++ show inputUser 
                                        ++ "is invalide, it should be a number."
                endGame   
    
    -- | Check the status of the game at the end of each round (Winner || NoWinner) 
    endRound :: StateT GameStates IO ()
    endRound = do 
        gs <- State.get
        case (gameStatus gs) of
            Winner -> do
                State.lift $ clearScreen
                printScoreboard
                printGWinner
                endGame
            NoWinner -> do
                printRWinner
                nextRound
    

    -- | Check the score of the player and set the score in the scoreboard.
    checkAndSetScore :: RPlayer -> Tries -> StateT GameStates IO ()
    checkAndSetScore rPlayer trie = do
        gs <- State.get
        let cNbTry = _nbOfTry gs 
        let (pID, pScore) = rPlayer 
        let (lID, lScore) = _rLeader gs  
        let pName = getPName pID $ _players gs  
        case (pScore > lScore) of
            True -> do
                setRLeader rPlayer
                setTrie (if cNbTry >= 1 && trie == 1 || trie == 3 && lScore /= NoRolls  then 1 else trie) -- need to be improve
                addPScore rPlayer
                printNewLMsg 
                otherPRolls    
            False -> addPScore rPlayer
    
                    
    -- | Function to distribut the number of token to the players
    tokenDistrib :: StateT GameStates IO ()
    tokenDistrib = do
        gs <- State.get
        -- get the players 
        let players = _players gs 
        -- get the ID of the player with the lower score
        let pLSId = fst $ minPScore (_scoreboard gs) 
        -- Calculate the token to distribut for each player except the player with the lower score
        let psToken = calcPToken (_scoreboard gs) pLSId
        -- Calculate the sum of the token to distribut to the player with the lower score
        let tokenToGive = sumPToken psToken 
        -- Add the token from the other players to the player with the lower score
        let pLScore = addToken pLSId players tokenToGive  
        -- Update the token value for each player
        let uPToken = pLScore : [(_id, name, token - (getPToken _id psToken)) 
                                |(_id, name, token) <- players,
                                _id /= pLSId]
    
        put $ gs {_players=uPToken}
    

    -- | Function to make a player rolls the dice a certain number of time 
    playerRolls :: Player -> Tries -> StateT GameStates IO ()
    playerRolls p@(pid, name, _) tries = do
        -- Show Scoreboard and number of tries the user have
        printScoreboard
        printTrieMsg name tries
        -- Rolls the dice
        _ <- State.lift $ getLine
        rolls <- State.lift $ rollThreeDice 
        State.lift $ drawTheDice rolls
        -- Create the score to add in the scoreboard
        let pScore = (pid, rolls)
        case tries of
            1 -> do
                checkAndSetScore pScore defTries
            _ -> do
                printLine
                State.lift . putStrLn $ name ++ ", do you want an other trie ? (Y/N):\n" 
                inputUser <- State.lift $ getAUTrie
                case inputUser of
                    "Y" -> playerRolls p (tries - 1)
                    "N" -> printLine >> checkAndSetScore pScore (if tries == defTries then 1 else tries)
    
    
    -- | Function to initialized the game state
    initGameState :: IO GameStates
    initGameState = do
        -- Get input from the user
        nbPlayers <- getINbPlayers
        nbToken <- getINbToken
        pNames <- getIPNames nbPlayers 
        let players = zip3 [1..nbPlayers] pNames (replicate nbPlayers nbToken) 
        -- Round 0
        clearScreen
        winnerR0 <- playRound0 players
        return $ GameStates 
            {
                _players=players,
                _round=1,
                _rLeader=defRLeader,
                _scoreboard=defScoreboard,
                _nbOfTry=defTries,
                _rWinner=winnerR0,
                _gStatus=NoWinner
            }
    
    {-- 
    Function to find the player to start the first play in the round 1.
    Each players rolls the dice, the player with the highest score win the round 0.
    --}
    playRound0 :: [Player] -> IO Player
    playRound0 players = do
        -- Rolls the dice for each player
        rolls <- sequence [rollsDice p | p <- players]
        -- Get the name of the player with the highest score
        let pid = fst $ maxPScore rolls
        let winner =  getPlayer pid players 
        -- Print the name of the winner and wait the winner to press enter to start the round 1
        printR0Winner $ getName winner 
        _ <- getLine
        clearScreen
        return winner
    
    -- | Function to make a round 
    playRound :: StateT GameStates IO ()
    playRound = do
        leaderRolls
        otherPRolls
        setRWinner
        tokenDistrib
        endRound
    
    -- | Function to initialize the data of the next round
    nextRound :: StateT GameStates IO ()
    nextRound = do
        clearScoreboard
        setRLeader defRLeader
        setTrie defTries
        addRound
        playRound
    
    -- | Function to restart the game with the same players and other token number
    restartGame :: StateT GameStates IO ()
    restartGame = do
      -- Initialize the game state 
      clearScoreboard
      setRLeader defRLeader
      setTrie defTries
      -- Set the number of token of each player
      gs <- State.get
      nbToken <- State.lift $ getINbToken 
      setPToken nbToken
      -- Round 0
      wPlayer <- State.lift $ playRound0 $ _players gs
      setWinner wPlayer
      playRound
    
    -- | Function to launch the game
    playGame :: IO ()
    playGame = do 
        initGS <- initGameState
        gs <- execStateT playRound initGS 
        return ()
  



    

            
            




    


    
    

    


        
 


  