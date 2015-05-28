module Router where

import Data.Char

data Action =
    Show Integer 
  | Edit Integer 
  | Update Integer 
  | Destroy Integer 
  | Index 
  | New 
  | Create 
  deriving (Eq, Show)

data Resources =
    Users
  | Posts
  | Comments
  deriving (Show, Eq)

data Route =
    Do Action Resources
  | In Resources Integer Route
  deriving (Show, Eq)


indexUsers :: Route
indexUsers = Do Index Users

inUser :: Integer -> Route -> Route
inUser = In Users

inPost :: Integer -> Route -> Route
inPost = In Posts


indexComments :: Route
indexComments = In Users 3 $ In Posts 2 $ Do Index Comments

createPost :: Route
createPost = In Users 2 $ Do Create Posts

showUserPost :: Integer -> Integer -> Route
showUserPost u n = In Users u $ Do (Show n) Posts


toPath :: Route -> String
toPath =  map toLower . concat . rest
    where
        rest (Do action res) = ["/", show res, (getPathPiece . getIdentifier) action]
        rest (In res ident r) = ["/", show res, "/", show ident, toPath r]


getAction :: Route -> Action
getAction (Do action _)  = action
getAction (In _ _ route) = getAction route


getIdentifier :: Action -> Maybe Integer
getIdentifier (Show n)    = Just n
getIdentifier (Edit n)    = Just n
getIdentifier (Update n)  = Just n
getIdentifier (Destroy n) = Just n
getIdentifier _           = Nothing

getPathPiece :: Maybe Integer -> String
getPathPiece (Just n) = "/" ++ show n
getPathPiece Nothing = ""
