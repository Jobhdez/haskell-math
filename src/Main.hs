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
import MatrixVectorClass (
  add,
  sub,
  mul)
import Matrix (
  Matrix(..)
  )
type API = "api" :> "matrix" :> "det" :> ReqBody '[JSON] ExprInfo :> Post '[JSON] DeterminantResponse
      :<|> "api" :> "matrix" :> "matTrace" :> ReqBody '[JSON] ExprInfo :> Post '[JSON] TraceResponse
      :<|> "api" :> "matrix" :> "matUpTriangular" :> ReqBody '[JSON] ExprInfo :> Post '[JSON] ExprInfo
      :<|> "api" :> "matrix" :> "matLowTriangular" :> ReqBody '[JSON] ExprInfo :> Post '[JSON] ExprInfo
      :<|> "api" :> "matrix" :> "exps" :> Get '[JSON] [ExpRecord]
      :<|> "api" :> "matrix" :> "exp" :> Capture "id" Int :> Get '[JSON] ExpRecord
      :<|> "api" :> "matrix" :> "exp" :> Capture "id" Int :> Delete '[JSON] ()
      :<|> "api" :> "matrix" :> "arithmetic" :> "addition" :> ReqBody '[JSON] MatrixArith :> Post '[JSON] ExprInfo
      :<|> "api" :> "matrix" :> "arithmetic" :> "subtraction" :> ReqBody '[JSON] MatrixArith :> Post '[JSON] ExprInfo
      :<|> "api" :> "matrix" :> "arithmetic" :> "multiplication" :> ReqBody '[JSON] MatrixArith :> Post '[JSON] ExprInfo
      :<|> "api" :> "matrix" :> "arithmetic" :> "exps" :> Get '[JSON] [ArithRecord]
data MatrixArith = MatrixArith {
  mexp :: [[Int]],
  mexp2 :: [[Int]]
  } deriving (Generic, Show)
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

data ArithRecord = ArithRecord {
  inputm :: Value,
  input2m :: Value,
  resultm :: Value
  } deriving (Generic, Show)

instance FromJSON ExprInfo
instance ToJSON ExprInfo

instance FromJSON MatrixArith
instance ToJSON DeterminantResponse

instance ToJSON TraceResponse

instance ToJSON ExpRecord
instance FromJSON ExpRecord

instance ToJSON ArithRecord
instance FromJSON ArithRecord

instance FromRow ExpRecord where
  fromRow = ExpRecord <$> field <*> field

instance FromRow ArithRecord where
  fromRow = ArithRecord <$> field <*> field <*> field 

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

addForClient :: MatrixArith -> ExprInfo
addForClient e =
  ExprInfo (getData exp')
  where
    exp' = add (Matrix (mexp e)) (Matrix (mexp2 e))

subForClient :: MatrixArith -> ExprInfo
subForClient e =
  ExprInfo (getData exp')
  where
    exp' = sub (Matrix (mexp e)) (Matrix (mexp2 e))

mulForClient :: MatrixArith -> ExprInfo
mulForClient e =
  ExprInfo (getData exp')
  where
    exp' = mul (Matrix (mexp e)) (Matrix (mexp2 e))
    

getData :: Matrix -> [[Int]]
getData (Matrix d) = d

lAlgServer :: Pool Connection -> Server API
lAlgServer pool = det
  :<|> matTrace
  :<|> matUpTriangular
  :<|> matLowTriangular
  :<|> getExps
  :<|> mathyExpGETById
  :<|> mathyExpDELETE
  :<|> additionPOST
  :<|> subtractionPOST
  :<|> multiplicationPOST
  :<|> arithExpsGET
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

  mathyExpGETById :: Int -> Servant.Handler ExpRecord
  mathyExpGETById id = do
    me <- liftIO $ withResource pool $ \conn ->
      query conn "SELECT input, result FROM exps WHERE id=?" (Only id)
    case me of
      [expRecord] -> return expRecord
      _           -> throwError err404

  mathyExpDELETE :: Int -> Servant.Handler ()
  mathyExpDELETE id = do
    liftIO $ withResource pool $ \conn ->
      execute conn "DELETE FROM exps WHERE id = ?" (Only id)
    return ()

  additionPOST :: MatrixArith -> Servant.Handler ExprInfo
  additionPOST  mat = do
    let result = addForClient mat
    liftIO $ withResource pool $ \conn ->
      execute conn "INSERT INTO matrix_exps (input, input2, result) VALUES (?, ?, ?)" (toJSON (mexp mat), toJSON (mexp2 mat), toJSON (expr result))
    return result

  subtractionPOST :: MatrixArith -> Servant.Handler ExprInfo
  subtractionPOST  mat = do
    let result = subForClient mat
    liftIO $ withResource pool $ \conn ->
      execute conn "INSERT INTO matrix_exps (input, input2, result) VALUES (?, ?, ?)" (toJSON (mexp mat), toJSON (mexp2 mat), toJSON (expr result))
    return result

  multiplicationPOST :: MatrixArith -> Servant.Handler ExprInfo
  multiplicationPOST  mat = do
    let result = mulForClient mat
    liftIO $ withResource pool $ \conn ->
      execute conn "INSERT INTO matrix_exps (input, input2, result) VALUES (?, ?, ?)" (toJSON (mexp mat), toJSON (mexp2 mat), toJSON (expr result))
    return result

  arithExpsGET :: Servant.Handler [ArithRecord]
  arithExpsGET = do
    exps <- liftIO $ withResource pool $ \conn ->
      query_ conn "SELECT input, input2, result FROM matrix_exps"
    return exps
userAPI :: Proxy API
userAPI = Proxy

app1 :: Pool Connection -> Application
app1 pool = serve userAPI (lAlgServer pool)

main :: IO ()
main = do
  pool <- myPool
  initDB connectionInfo
  run 8081 (app1 pool)
