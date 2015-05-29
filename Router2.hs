{-# LANGUAGE GADTs #-}

module Router2 where

data Action a where
    Show   :: Resource a -> Action a 
    Edit   :: Resource a -> Action a
    Index  :: Resources  -> Action a
    Create :: Resources  -> Action a
    deriving (Eq, Show)

instance Functor Action where
    fmap f (Show a)   = Show (fmap f a)
    fmap f (Edit a)   = Edit (fmap f a)
    fmap _ (Index a)  = Index a
    fmap _ (Create a) = Create a

data Resources =
    Users
  | Posts
  | Comments
  deriving (Eq, Show)

type Resource a = (Resources, a)

data Route =
    Do Resources
  | In Resources Route
  deriving (Show, Eq)

(<//>) :: Resources -> Route -> Route 
(<//>) = In

infixr 0 <//>

routes :: [Route]
routes =
    [ Do Users
    , Users <//> Do Posts
    , Users <//> Posts <//> Do Comments
    ]
