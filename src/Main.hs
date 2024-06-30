{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
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
--import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

import LinearAlgebra (
  determinant,
  trace,
  upperTriangular,
  lowerTriangular
  )

type API = "det" :> ReqBody '[JSON] ExprInfo :> Post '[JSON] DeterminantResponse
      :<|> "matTrace" :> ReqBody '[JSON] ExprInfo :> Post '[JSON] TraceResponse
      :<|> "matUpTriangular" :> ReqBody '[JSON] ExprInfo :> Post '[JSON] ExprInfo
      :<|> "matLowTriangular" :> ReqBody '[JSON] ExprInfo :> Post '[JSON] ExprInfo
      :<|> "exps" :> Get '[JSON] [ExpRecord]
      :<|> "mexp" :> Capture "id" Int :> Get '[JSON] ExpRecord

data ExprInfo = ExprInfo {
  expr :: [[Int]]
  } deriving (Generic, Show)

data DeterminantResponse = DeterminantResponse {
  detexp :: Int
  } deriving (Generic, Show)

data TraceResponse = TraceResponse {
  traceExp :: Int
  } deriving (Generic, Show)

data ExpRecord = ExpRecord {
  input :: Value,
  result :: Value
  } deriving (Generic, Show)

instance FromJSON ExprInfo
instance ToJSON ExprInfo

instance ToJSON DeterminantResponse

instance ToJSON TraceResponse

instance ToJSON ExpRecord
instance FromJSON ExpRecord

instance FromRow ExpRecord where
  fromRow = ExpRecord <$> field <*> field

connectionInfo :: ConnectInfo
connectionInfo =
  defaultConnectInfo { connectHost = "127.0.0.1",
                       connectPort = 5432,
                       connectUser = "haskell2",
                       connectPassword = "hello123",
                       connectDatabase= "haskell2"
                     }

initDB :: ConnectInfo -> IO ()
initDB conn = bracket (connect conn) close $ \con -> do
  _ <- execute_ con "CREATE TABLE IF NOT EXISTS exps (id SERIAL, input JSONB, result JSONB)"
  return ()

myPool :: IO (Pool Connection)
myPool = createPool (connect connectionInfo) close 1 10 10

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
  
lAlgServer :: Pool Connection -> Server API
lAlgServer pool = det
  :<|> matTrace
  :<|> matUpTriangular
  :<|> matLowTriangular
  :<|> getExps
  :<|> mexp
  where
  det :: ExprInfo -> Servant.Handler DeterminantResponse
  det clientInfo = do
    let result = exprForClient clientInfo
    liftIO $ withResource pool $ \conn ->
      execute conn "INSERT INTO exps (input, result) VALUES (?, ?)" (toJSON (expr clientInfo), toJSON (detexp result))
    return result

  matTrace :: ExprInfo -> Servant.Handler TraceResponse
  matTrace clientInfo = do
    let result = traceForClient clientInfo
    liftIO $ withResource pool $ \conn ->
      execute conn "INSERT INTO exps (input, result) VALUES (?, ?)" (toJSON (expr clientInfo), toJSON (traceExp result))
    return result

  matUpTriangular :: ExprInfo -> Servant.Handler ExprInfo
  matUpTriangular clientInfo = do
    let result = upTriangularForClient clientInfo
    liftIO $ withResource pool $ \conn ->
      execute conn "INSERT INTO exps (input, result) VALUES (?, ?)" (toJSON (expr clientInfo), toJSON (expr result))
    return result

  matLowTriangular :: ExprInfo -> Servant.Handler ExprInfo
  matLowTriangular clientInfo = do
    let result = lowTriangularForClient clientInfo
    liftIO $ withResource pool $ \conn ->
      execute conn "INSERT INTO exps (input, result) VALUES (?, ?)" (toJSON (expr clientInfo), toJSON (expr result))
    return result

  getExps :: Servant.Handler [ExpRecord]
  getExps = do
    exps <- liftIO $ withResource pool $ \conn ->
      query_ conn "SELECT input, result FROM exps"
    return exps

  mexp :: Int -> Servant.Handler ExpRecord
  mexp id = do
    me <- liftIO $ withResource pool $ \conn ->
      query conn "SELECT input, result FROM exps WHERE id=?" (Only id)
    case me of
      [expRecord] -> return expRecord
      _           -> throwError err404

userAPI :: Proxy API
userAPI = Proxy

app1 :: Pool Connection -> Application
app1 pool = serve userAPI (lAlgServer pool)

main :: IO ()
main = do
  pool <- myPool
  initDB connectionInfo
  run 8081 (app1 pool)
