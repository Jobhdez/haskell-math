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

import LinearAlgebra (
  determinant,
  trace,
  upperTriangular,
  lowerTriangular
  )

type API = "det" :> ReqBody '[JSON] ExprInfo :> Post '[JSON] DeterminantResponse :<|> "matTrace" :> ReqBody '[JSON] ExprInfo :> Post '[JSON] TraceResponse :<|> "matUpTriangular" :> ReqBody '[JSON] ExprInfo :> Post '[JSON] ExprInfo :<|> "matLowTriangular" :> ReqBody '[JSON] ExprInfo :> Post '[JSON] ExprInfo

data ExprInfo = ExprInfo {
  expr :: [[Int]]
  } deriving Generic

data DeterminantResponse = DeterminantResponse {
  exp :: Int
  } deriving Generic

data TraceResponse = TraceResponse {
  traceExp :: Int
  } deriving Generic

instance FromJSON ExprInfo
instance ToJSON ExprInfo

instance ToJSON DeterminantResponse

instance ToJSON TraceResponse


exprForClient :: ExprInfo -> DeterminantResponse
exprForClient e = DeterminantResponse exp' where
  exp' = determinant (expr e)

traceForClient :: ExprInfo -> TraceResponse
traceForClient e = TraceResponse exp' where
  exp' = trace (expr e)

upTriangularForClient :: ExprInfo -> ExprInfo
upTriangularForClient e = ExprInfo exp' where
  exp' = upperTriangular (expr e)

lowTriangularForClient :: ExprInfo -> ExprInfo
lowTriangularForClient e = ExprInfo exp' where
  exp' = lowerTriangular (expr e)
  
lAlgServer :: Server API
lAlgServer = det
  :<|> matTrace
  :<|> matUpTriangular
  :<|> matLowTriangular
  where
  det :: ExprInfo -> Handler DeterminantResponse
  det clientInfo = return (exprForClient clientInfo)

  matTrace :: ExprInfo -> Handler TraceResponse
  matTrace clientInfo = return (traceForClient clientInfo)

  matUpTriangular :: ExprInfo -> Handler ExprInfo
  matUpTriangular clientInfo = return (upTriangularForClient clientInfo)

  matLowTriangular :: ExprInfo -> Handler ExprInfo
  matLowTriangular clientInfo = return (lowTriangularForClient clientInfo)

userAPI :: Proxy API
userAPI = Proxy

app1 :: Application
app1 = serve userAPI lAlgServer

main :: IO ()
main = run 8081 app1
