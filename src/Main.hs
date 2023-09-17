{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

import LinearAlgebra

type API = "compute" :> ReqBody '[JSON] ExprInfo :> Post '[JSON] DeterminantResponse

data ExprInfo = ExprInfo {
  expr :: [[Int]]
  } deriving Generic

data DeterminantResponse = DeterminantResponse {
  exp :: Int
  } deriving Generic

instance FromJSON ExprInfo
instance ToJSON ExprInfo

instance FromJSON DeterminantResponse
instance ToJSON DeterminantResponse

exprForClient :: ExprInfo -> DeterminantResponse
exprForClient e = DeterminantResponse expr' where
  expr' = determinant (expr e)


lAlgServer :: Server API
lAlgServer = compute where
  compute :: ExprInfo -> Handler DeterminantResponse
  compute clientInfo = return (exprForClient clientInfo)
              

userAPI :: Proxy API
userAPI = Proxy

app1 :: Application
app1 = serve userAPI lAlgServer

main :: IO ()
main = run 8081 app1
