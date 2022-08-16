module Playground where
    import Control.Monad


    data Rolls a = Rolls a | NoRolls deriving (Show)


    instance Functor Rolls where
        fmap f (Rolls x) = Rolls (f x)
    instance Applicative Rolls where 
        pure x = Rolls x
        Rolls f <*> Rolls x = Rolls (f x)
    instance Monad Rolls where 
        return = pure
        Rolls x >>= f = f x
        