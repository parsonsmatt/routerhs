module Router where

import Data.Char

data Action a =
    Show a 
  | Edit a 
  | Update a 
  | Destroy a 
  | Index 
  | New 
  | Create 
  deriving (Eq, Show)

instance Functor Action where
    fmap f (Show a)     = Show (f a)
    fmap f (Edit a)     = Edit (f a)
    fmap f (Update a)   = Update (f a)
    fmap f (Destroy a)  = Destroy (f a)
    fmap _ Index        = Index
    fmap _ New          = New
    fmap _ Create       = Create

data Resources =
    Users
  | Posts
  | Comments
  deriving (Show, Eq)

data Route a =
    Do (Action a) Resources
  | In Resources a (Route a)
  deriving (Show, Eq)

instance Functor Route where
    fmap f (Do action res)  = Do (fmap f action) res
    fmap f (In res x route) = In res (f x) (fmap f route)

indexUsers :: Route a
indexUsers = Do Index Users

inUser :: a -> Route a -> Route a
inUser = In Users

inPost :: a -> Route a -> Route a
inPost = In Posts


indexComments :: Num a => Route a 
indexComments = In Users 3 $ In Posts 2 $ Do Index Comments

createPost :: Num a => Route a 
createPost = In Users 2 $ Do Create Posts

showUserPost :: Num a => a -> a -> Route a
showUserPost u n = In Users u $ Do (Show n) Posts


toPath :: Show a => Route a -> String
toPath =  map toLower . concat . rest
    where
        rest (Do action res) = ["/", show res, (getPathPiece . getIdentifier) action]
        rest (In res ident r) = ["/", show res, "/", show ident, toPath r]


getAction :: Route a -> Action a
getAction (Do action _)  = action
getAction (In _ _ route) = getAction route


getIdentifier :: Action a -> Maybe a
getIdentifier (Show n)    = Just n
getIdentifier (Edit n)    = Just n
getIdentifier (Update n)  = Just n
getIdentifier (Destroy n) = Just n
getIdentifier _           = Nothing

getPathPiece :: (Show a) => Maybe a -> String
getPathPiece (Just n) = "/" ++ show n
getPathPiece Nothing = ""
