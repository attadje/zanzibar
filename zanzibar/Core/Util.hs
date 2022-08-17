module Core.Util where
    import Core.Types
    import System.Random
    import Control.Monad
    import Core.Config
    import Data.List (sort, sortBy)
    import Control.Monad.State as State
    import Data.List
    import Data.Function (on)
    import Data.Ord


   -- | ###########################################################################################
   -- | ############################## Player data functions ######################################
   -- | ###########################################################################################

   -- Function to find if the player is the current leader
    isLeader :: Player -> RPlayer -> Bool
    isLeader (pid, _, _) (pRPid, _) = pid == pRPid
    
    -- | Function to get the player data by the player id
    getPlayer :: PID -> Players -> Player
    getPlayer id players = head [player | player@(_id, _, _) <- players , id == _id] 
    
    -- | Function to get the player name from the player data
    getName :: Player -> PName
    getName (_, name, _) = name
    
    -- | Function to get the player id from the player data 
    getPID :: Player -> PID
    getPID (id, _, _) = id

    -- | Function to get the the player with the lower score
    minPScore :: [RPlayer] -> RPlayer
    minPScore = foldr1 (\p1@(_,rolls1) p2@(_,rolls2) -> if rolls1 < rolls2 then p1 else p2) 
    
    -- | Function to get the the player with the highest score
    maxPScore :: [RPlayer] -> RPlayer
    maxPScore = foldr1 (\p1@(_,r1) p2@(_,r2) -> if r1 > r2 then p1 else p2)

    -- | Function to check if a player have a greater ID than an other
    sortPID :: Player -> Player -> Ordering
    sortPID a b 
        | getPID a > getPID b = GT
        | otherwise           = LT

    -- | Function to get the player name with the player ID
    getPName :: PID -> Players -> PName
    getPName id players = head [name | (_id, name, _) <- players, id == _id]

   
   -- | ###########################################################################################
   -- | ############################## Get game state informations ################################
   -- | ###########################################################################################
    
    -- | Function to get the round leader
    getRLeader :: GameStates -> RPlayer
    getRLeader = \gs -> _rLeader gs
    
    -- | Function to check if a player have already scored in the round
    isPScored :: Player -> [PID] -> Bool
    isPScored (id, _, _) pid 
        | id `elem` pid = True
        | otherwise     = False

     {-- 
    Function to get the game winner(s).
    The player(s) with the number of token to 0 can pretend to have win the game. 
    --}
    getWinnerID :: Players -> [PID]
    getWinnerID sb = [_id | (_id, _, token) <- sb, token <= 0] 
    
    
    {-- 
    Function to get the winner of the game. 
    If several winners, the player with the highest score win the game.
    Imprve this function for handle the case of multiple winners.
    If multiple winner make a round to with one trie for each player and get the winner.
    --}
    getGWinner :: GameStates -> Player
    getGWinner gs = winner 
        where 
            psID     = getWinnerID (_players gs) 
            winnerID = fst . maxPScore $ [p | p@(pid, _) <- _scoreboard gs, pid `elem` psID]
            winner   = getPlayer winnerID (_players gs) 


    -- | Function to pick a player to play next 
    pickPToPlay :: GameStates -> Either Player String
    pickPToPlay gs = go players pID 
        where 
            -- Get the list of the players ordering by based on the last round score
            playOrder :: Players
            playOrder = _playOrder gs
            -- Get the player ordering by the score of the last round or the id number
            players = if playOrder == [] then _players gs else playOrder  
            -- Get the ID of the player who have scored
            pID = sort [fst pScore | pScore <- _scoreboard gs] 
            -- Function to find a player who have not scored 
            go :: Players -> [PID] -> Either Player String
            go [] _ = Right "Empty" 
            go (player:players) pid 
                | isPScored player pid  = go players pid
                | otherwise        = Left player

    -- | Function to get the status (Winner | NoWinner) of the game
    gameStatus :: GameStates -> GameStatus
    gameStatus gs = if length winners == 0 then NoWinner else Winner
        where
            players = _players gs 
            winners = getWinnerID players


    -- | Function to get rolls of a player using the id 
    getRollsbyId :: PID -> Scoreboard -> Rolls
    getRollsbyId pid [] = Rolls [] 
    getRollsbyId pid ps@(x:xs) = rolls
        where 
        l = [rolls | (_pid, rolls) <- ps, _pid == pid]
        rolls = if l == [] then Rolls [] else head l


   -- | ############################################################################################
   -- | ############################## Set game state informations #################################
   -- | ############################################################################################

    -- | Function to set the play order of the next round
    setNextRoundPO :: StateT GameStates IO ()
    setNextRoundPO = do
        gs <- State.get
        let scoreboard = descPScore $ _scoreboard gs
        let playerOrd  = [getPlayer _id (_players gs) | (_id,_) <-scoreboard]
        put $ gs {_playOrder=playerOrd}


    -- | Function to set the round leader 
    setRLeader :: RPlayer -> StateT GameStates IO ()
    setRLeader p = do 
        gs <- State.get
        put $ gs {_rLeader=p}
    
    -- | Function to increase the number of round 
    addRound :: StateT GameStates IO ()
    addRound = do
        gs <- State.get
        put $ gs {_round=(_round gs) + 1}
    
    -- | Function to add the player score to the scoreboard
    addPScore :: RPlayer -> StateT GameStates IO ()
    addPScore score = do
        gs <- State.get
        put $ gs {_scoreboard=lookAR score (_scoreboard gs)}
    
    -- | Function to clear the scoreboard
    clearScoreboard :: StateT GameStates IO ()
    clearScoreboard = do
        gs <- State.get
        put $ gs {_scoreboard=[]}
    
    -- | Function to set the number of round
    setRound :: Int -> StateT GameStates IO ()
    setRound nb = do
        gs <- State.get
        put $ gs {_round=nb}

    -- | Function to set the round leader as the winner of the round 
    setRWinner :: StateT GameStates IO ()
    setRWinner = do
        gs <- State.get
        let (id,_) = _rLeader gs  
        put $ gs {_rWinner=getPlayer id (_players gs)}
    
    -- | Function to set the number of trie
    setTrie :: Int -> StateT GameStates IO ()
    setTrie nb = do
        s <- State.get
        put $ s {_nbOfTry=nb}


    -- | Function to set the round winner
    setWinner :: (Int, String, Int) -> StateT GameStates IO ()
    setWinner p = do
        gs <- State.get
        put $ gs {_rWinner=p} 

    
    -- | Function to give the same number of token to each player 
    setPToken :: Int -> StateT GameStates IO ()
    setPToken nbToken = do
        gs <- State.get
        put $ gs {_players=[(id, name, nbToken) | (id,name,_) <- _players gs]}



   -- | ##################################################################################################
   -- | ############################## Token distribution functions ######################################
   -- | ##################################################################################################

    {--
    Function to calculate the number of token each player will give to the player with the lower score.
    The number of token is calculated according to the player combination.
    --}
    calcPToken :: Scoreboard -> PID -> [(PID, Token)] 
    calcPToken sb id = [(_id, tokenToGive score) 
                       |(_id, score) <- sb,
                       _id /= id] -- Remove the id of the player with the lower score
    
    -- Function to calculate the total of token to give to the player with the lower score
    sumPToken :: [(PID, Token)] -> Int
    sumPToken pToken = sum [p | (_,p) <- pToken]
    
    -- Function to get the player token from the player id 
    getPToken :: PID -> [(PID, Token)] -> Token 
    getPToken id pToken = head [t | (_id,t) <- pToken, id == id]
    
    -- Function to add token to a player
    addToken :: PID -> Players -> Token -> Player
    addToken id players token = head [(_id, name, pToken + token) 
                                     |(_id, name, pToken) <- players,
                                     id == _id]
                                     
                                     
    -- | Function to determinate the number of token to give based on a score
    tokenToGive :: Rolls -> Int
    tokenToGive pRolls 
        | pRolls `elem` zanzibar = 5  -- Highest Score
        | pRolls `elem` hrc      = 3  -- Highest Ranking Score
        | pRolls `elem` sc       = 2  -- Special Score
        | otherwise              = 1  -- Other Score



   -- | #############################################################################
   -- | ############################## Other functions ############################## 
   -- | #############################################################################

    -- Convert an int to a dice number
    intToDiceNb :: Int -> DiceNb
    intToDiceNb diceNb = case diceNb of
        1 -> One
        2 -> Two
        3 -> Three
        4 -> Four
        5 -> Five
        6 -> Six

    -- Function to generate n random number 
    genNRandomNb :: Int -> IO [Int]
    genNRandomNb n  = replicateM n (randomRIO (1, 6))


    {-- 
    "Look And Replace" 
    This function replace a tuple in a list of tupple.
    The function add the new tuple in the first element of the list then
    add a the tuple list with the old tuple removed from the list. 
    --}
    lookAR :: Eq a => (a, b) -> [(a, b)] -> [(a, b)]
    lookAR newT = \listT -> newT : [t | t@(e, _) <- listT, e /= fst newT]


    -- | Function to get the value from the Left context
    left :: Either a b -> a
    left (Left a) = a

    -- | Function to format the result of the rolls (Ex: One - Two - Three)
    formatScore :: Rolls -> String
    formatScore (Rolls []) = "-"
    formatScore (Rolls l)  = intercalate "-" [show nb | nb <- l]

    -- | Ordering the player score from the lowest to the highest score
    ascPScore :: Scoreboard -> Scoreboard 
    ascPScore = sort
    -- | Ordering the player score from the highest to the lowest score
    descPScore :: Scoreboard -> Scoreboard
    descPScore = sortOn (Down . snd)

    -- | Get the third elem of a list
    thrd :: (a, b, c) -> c
    thrd (_,_,c) = c

