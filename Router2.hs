{-# LANGUAGE GADTs #-}

module Router2 where

data Action a where
    Show :: Resource a -> Action a 
    Edit :: Resource a -> Action a
    Index :: Resources -> Action a
    Create :: Resources -> Action a
    deriving (Eq, Show)

data Resources =
    Users
  | Posts
  | Comments
  deriving (Eq, Show)

type Resource a = (Resources, a)

data Route a =
    Do (Action a)
  | In (Resource a) (Route a)
  deriving (Show, Eq)

infixl 3 `In`
