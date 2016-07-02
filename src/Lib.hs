{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( app1
    , main'
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Data.Aeson
import GHC.Generics

import Data.Time.Calendar
import Data.List (sortBy)

------------------------ API SEPC ---------------------------
type UserAPI = 
               "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]
          :<|> "albert" :> Get '[JSON] User
          :<|> "isaac" :> Get '[JSON] User

data User = User
    { name :: String
    , age :: Int
    , email :: String
    , registration_date :: Day
    } deriving (Eq, Show, Generic)

instance ToJSON User

-- Derivin Generic gives a FromText instance ? FromHttpApi specifies how to decode a query param into a 
-- value a given type with: parseQueryParam :: Text -> Either Text a
data SortBy = Age | Name deriving (Generic, Show,Eq)

instance FromHttpApiData SortBy where
    parseQueryParam "age" = Right Age
    parseQueryParam "name" = Right Name
    parseQueryParam _ = Left "Not valid query param"

---------------------- HANDLERS -------------------
users :: [User]
users =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)
  , User "Haskell Curry" 200 "hs@curry.org" (fromGregorian 1900 9 12)
  ]

albert = users !! 0
isaac = users !! 1

compareWith f x y = compare (f x) (f y) 

sortedUsers Age = sortBy (compareWith age)
sortedUsers Name = sortBy (compareWith name)

-- m is actually a EitherT ServantErr IO, in the abscense of SortBy query param users gets returned
sortUsers :: Monad m =>  Maybe SortBy -> m [User]
sortUsers = return . maybe users (flip sortedUsers users)

server1 :: Server UserAPI
server1 = 
          sortUsers
     :<|> return isaac
     :<|> return albert

userAPI :: Proxy UserAPI
userAPI = Proxy

---------------------------------- SERVER --------------------------------
{- 'serve' comes from servant and hands you a WAI Application,
which you can think of as an "abstract" web application,
not yet a webserver. -}

app1 :: Application
app1 = serve userAPI server1

main' = do 
   putStrLn "Server running in http://localhost:8081"
   run 8081 app1
