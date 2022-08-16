module Core.Config where
    import Core.Types

    -- Zanzibar (Highest  score)
    zanzibar :: [Rolls]
    zanzibar = [Rolls [Four, Five, Six]]

    -- Highest ranking score
    hrc :: [Rolls]
    hrc = [Rolls [One, One, One],
           Rolls [Two, Two, Two],
           Rolls [Three, Three, Three],
           Rolls [Four, Four, Four],
           Rolls [Five, Five, Five],
           Rolls [Six, Six, Six]]
           
    -- Special score
    sc :: [Rolls]
    sc = [Rolls [One, Two, Three]]
    
    -- Tries number by default
    defTries :: Int
    defTries = 3
    
     -- Round player data by default
    defRLeader :: RPlayer
    defRLeader = (0, NoRolls)
    
    -- Scoreboard by default
    defScoreboard :: [RPlayer]
    defScoreboard = []





    

    

