module Core.Types where
    import System.Random
    import Control.Monad.State
    
    type PID = Int
    type Tries = Int
    type Token = Int
    type PToken = Int
    type PName = String
    type Players = [Player]
    type PlayOrder = [Player]
    type RPlayer = (PID, Rolls)
    type Scoreboard = [(PID, Rolls)]
    type Player = (PID, PName, PToken)
    
    data GameStatus = Winner | NoWinner deriving Show
    
    data GameStates = GameStates 
        {
            _players       :: Players,
            _round         :: Int,
            _rLeader       :: RPlayer,   -- Round leader
            _scoreboard    :: [RPlayer],
            _playOrder     :: [Player], 
            _nbOfTry       :: Int,       
            _rWinner       :: Player,    -- Round winner
            _gStatus       :: GameStatus -- Game status 
        }  deriving (Show)
        
    data DiceNb 
        = One 
        | Two 
        | Three 
        | Four 
        | Five 
        | Six 
        deriving (Show)
    
    data Rolls 
        = Rolls [DiceNb] | NoRolls deriving (Show)
    
    instance Eq Rolls where
        (==) a b = 
            let 
                rolls NoRolls                           = 0
                rolls (Rolls [One, One, One])           = 1
                rolls (Rolls [Two, Two, Two])           = 2
                rolls (Rolls [Three, Three, Three])     = 3
                rolls (Rolls [Four, Four, Four])        = 4
                rolls (Rolls [Five, Five, Five])        = 5
                rolls (Rolls [Six, Six, Six])           = 6
                rolls rolls                             = calcScore rolls
            in 
                (rolls a) == (rolls b)
    
    instance Ord Rolls where
        compare a b =
            let 
                rolls NoRolls                       = 0
                rolls (Rolls [Four, Five, Six])     = 8 -- Zanzibar 
                rolls (Rolls [One, One, One])       = 7 
                rolls (Rolls [Two, Two, Two])       = 6
                rolls (Rolls [Three, Three, Three]) = 5
                rolls (Rolls [Four, Four, Four])    = 4
                rolls (Rolls [Five, Five, Five])    = 3
                rolls (Rolls [Six, Six, Six])       = 2
                rolls (Rolls [One, Two, Three])     = 1
                rolls rolls                         = (((calcScore rolls) * 99) / 260) / 100 -- (Value > 0 and <= 0.99)
            in 
                compare (rolls a) (rolls b)
    
    
    class CalcScore a where
        calcScore :: (Fractional b) => a -> b 
    
    instance CalcScore Rolls where
        calcScore (Rolls [a, b, c]) = 
            let score One     = 100
                score Two     = 2
                score Three   = 3
                score Four    = 4
                score Five    = 5
                score Six     = 60
            in (score a) + (score b) + (score c) 




        


    



  

       


    



  
    


