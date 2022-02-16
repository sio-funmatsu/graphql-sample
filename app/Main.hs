{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Control.Monad ( (<=<) )
import Data.Morpheus ( interpreter )
import Data.Morpheus.Types
    ( RootResolver(..), GQLType, Undefined(..), ResolverQ )
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.List ( find )
import GHC.Generics ( Generic )
import Network.Wai
    ( responseLBS, getRequestBodyChunk, Application )
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)


data User = User
    { name :: Text
    , email :: Text
    } deriving (Generic, GQLType)

newtype UserArgs = UserArgs
    { name :: Text
    } deriving (Generic, GQLType)

newtype Query m = Query
    { user :: UserArgs -> m User
    } deriving (Generic, GQLType)

userDB :: [User]
userDB =
    [ User "Sio0" "good.saltydog+0@gmail.com"
    , User "Sio1" "good.saltydog+1@gmail.com"
    , User "Sio2" "good.saltydog+2@gmail.com"
    , User "Sio3" "good.saltydog+3@gmail.com"
    , User "Sio4" "good.saltydog+4@gmail.com"
    , User "Sio5" "good.saltydog+5@gmail.com"
    ]

findUserByName :: Text -> Maybe User
findUserByName name = find (\(User name' _) -> name == name') userDB

resolveUser :: UserArgs -> ResolverQ e IO User
resolveUser (UserArgs name) = maybe (fail "User not found.") return $ findUserByName name

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver = RootResolver
    { queryResolver = Query { user = resolveUser }
    , mutationResolver = Undefined
    , subscriptionResolver = Undefined
    }

gqlApi :: ByteString -> IO ByteString
gqlApi = interpreter rootResolver

app :: Application
app request respond = do
    body <- getRequestBodyChunk request
    respond . responseLBS status200 [("Content-Type", "text/plain")] <=< gqlApi $ B.fromStrict body

main :: IO ()
main = do
    putStrLn "http://localhost:8888/"
    run 8888 app
