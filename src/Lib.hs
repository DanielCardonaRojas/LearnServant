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
    ( app
    , main'
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Data.Aeson
import GHC.Generics
import Data.String.Conversions
import Text.Read (readMaybe)
import Servant.HTML.Lucid
import Lucid
import Data.Time.Calendar
import Data.List (sortBy)
import Data.Complex
------------------------ API SPEC ---------------------------
type UserAPI = 
           "users" :> QueryParam "sortby" SortBy :> Get '[JSON, HTML] [User]
      :<|> "albert" :> Get '[JSON] User
      :<|> "isaac" :> Get '[JSON] User

type MathAPI = 
          "sum" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Int
     :<|> "conjugate" :> Capture "x" (Complex Int)  :> Get '[JSON] String

type MyAPI = UserAPI :<|> MathAPI

data User = User
    { name :: String
    , age :: Int
    , email :: String
    , registration_date :: Day
    } deriving (Eq, Show, Generic)

instance ToJSON User

instance ToHtml User where
    toHtml u = tr_ $ do
                td_ $ toHtml (name u)
                td_ $ toHtml (show $ age u)
                td_ $ toHtml (email u)

    toHtmlRaw = toHtml

instance ToHtml [User] where
    toHtml u = table_ $ do
        tr_ $ do
            th_ $ toHtml ("Name"::String)
            th_ $ toHtml ("Age"::String)
            th_ $ toHtml ("Email"::String)
        foldMap toHtml u

    toHtmlRaw = toHtml

data SortBy = Age | Name deriving (Generic, Show,Eq)

instance FromHttpApiData SortBy where
    parseQueryParam "age" = Right Age
    parseQueryParam "name" = Right Name
    parseQueryParam _ = Left "Not valid query param"

instance (Num a,Read a) => FromHttpApiData (Complex a) where
    --parseUrlPiece :: Text -> Either Text a
    parseUrlPiece txt = maybe (Left "Cant parse complex num") (Right) (readMaybe $ cs txt)
---------------------- HANDLERS -------------------
users :: [User]
users =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)
  , User "Haskell Curry" 200 "hs@curry.org" (fromGregorian 1900 9 12)
  ]

albert = users !! 0
isaac = users !! 1

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

server2 :: Server MathAPI
server2 = sum
      :<|> showConjugate
      where
       sum = return <.. (+)

       showConjugate :: Monad m => Complex Int -> m String
       showConjugate = return . show . conjugate

server :: Server MyAPI
server = server1 :<|> server2

myAPI :: Proxy MyAPI
myAPI = Proxy

---------------------------------- SERVER --------------------------------
{- 'serve' comes from servant and hands you a WAI Application,
which you can think of as an "abstract" web application,
not yet a webserver. -}

app :: Application
app = serve myAPI server

main' = do 
   putStrLn "Server running in http://localhost:8081"
   run 8081 app

----------------------------- UTILS -----------------------------
(<..) = (.) . (.)
compareWith f x y = compare (f x) (f y) 
