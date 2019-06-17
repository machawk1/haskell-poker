{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main  where

import Prelude 

import Control.Lens       hiding ((.=))
import Data.Aeson         (FromJSON, ToJSON)
import Data.Maybe         (fromMaybe)
import Data.String        (IsString (..))
import Data.Text          (Text)
import GHC.Generics       (Generic)
import Network.Wai        (Application)
import System.Environment (getArgs, lookupEnv)
import Text.Read          (readMaybe)

import Data.Swagger
import Servant
import Servant.Auth.Swagger

import Servant.Swagger
import Servant.Swagger.UI
import Servant.Swagger.UI.Core
import Servant.Swagger.UI.JensOleG
import Servant.Swagger.UI.ReDoc

import qualified Network.Wai.Handler.Warp as Warp


import GHC.Generics
import GHC.TypeLits


import API
import Types

-- swagger instances

instance ToParamSchema ReturnToken
instance ToSchema ReturnToken



-- To test nested case
type API' = API '[ JWT]
    :<|> "nested" :> API '[ JWT]
    :<|> SwaggerSchemaUI' "foo-ui" ("foo" :> "swagger.json" :> Get '[JSON] Swagger)

-- Implementation

-- | We test different ways to nest API, so we have an enumeration
data Variant
    = Normal
    | Nested
    | SpecDown
    deriving (Eq)

data UIFlavour
    = Original
    | JensOleG
    | ReDoc
    deriving (Eq)

server' :: UIFlavour -> Server API'
server' uiFlavour = server Normal
    :<|> server Nested
    :<|> schemaUiServer (swaggerDoc' SpecDown)
  where
    server :: Variant -> _
    server variant =
        schemaUiServer (swaggerDoc' variant) 
      where
        usersEndpoint _ _ = return Username (variant == Normal)
        -- Unfortunately we have to specify the basePath manually atm.

    schemaUiServer
        :: (Server api ~ Handler Swagger)
        => Swagger -> Server (SwaggerSchemaUI' dir api)
    schemaUiServer = case uiFlavour of
        Original -> swaggerSchemaUIServer
        JensOleG -> jensolegSwaggerSchemaUIServer
        ReDoc    -> redocSchemaUIServer

    swaggerDoc' Normal    = swaggerDoc
    swaggerDoc' Nested    = swaggerDoc
        & basePath ?~ "/nested"
        & info.description ?~ "Nested API"
    swaggerDoc' SpecDown  = swaggerDoc
        & info.description ?~ "Spec nested"

-- Boilerplate

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy (API '[ JWT]))
    & info.title       .~ "Cats API"
    & info.version     .~ "2016.8.7"
    & info.description ?~ "This is an API that tests servant-swagger support"

docsApi :: Proxy API'
docsApi = Proxy

app :: UIFlavour -> Application
app = serve docsApi . server'

main :: IO ()
main = do
    args <- getArgs
    let uiFlavour | "jensoleg" `elem` args = JensOleG
                  | "redoc"    `elem` args = ReDoc
                  | otherwise              = Original
    p <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
    putStrLn $ "http://localhost:" ++ show p ++ "/"
    Warp.run p (Main.app uiFlavour)
