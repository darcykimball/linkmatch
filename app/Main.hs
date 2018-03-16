{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import Data.Aeson
import Data.Monoid ((<>))
import Network.Simple.TCP
import Options.Applicative
import System.IO

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Broker
import Subscriber
import Publisher
import EqMatchTree
import Event


main :: IO ()
main = do
  -- XXX: Mainly for debugging
  hSetBuffering stdout NoBuffering

  modeCfg <- execParser opts
  print modeCfg    
  start modeCfg
  where
    opts = info (brokerOpts <|> pubOpts <|> subOpts)
      (fullDesc <> progDesc "A pub/sub node daemon.")


--
-- Actual daemon run stuff 
--


start :: ModeCfg -> IO ()
start BrokerCfg{..} = do
  -- Read in topology file
  schema <- readSchema _brokerSchemaPath

  error "TODO"

start SubCfg{..} = do
  -- Read in subs file
  schema <- readSchema _subSchemaPath

  -- Read in predicates
  preds <- readPreds _subPredsPath

  runSubscriber preds _subIP _subPort _subBrokerIP _subBrokerPort


start PubCfg{..} = do
  -- Read in schema
  schema <- readSchema _pubSchemaPath

  runPublisher schema _pubBrokerIP _pubBrokerPort (ringN 3)

--
-- Util
--


readSchema :: FilePath -> IO EventSchema
readSchema path = do
  schemaText <- B.readFile path

  maybe
    (error "Bad schema file!")
    return 
    (decode $ LB.fromStrict schemaText)
  

readPreds :: FilePath -> IO [Predicate]
readPreds path = do
  predsText <- B.readFile path

  maybe
    (error "Bad schema file!")
    return 
    (decode (LB.fromStrict predsText) :: Maybe [Predicate])


--
-- Command-line parsing stuff
-- 


brokerOpts :: Parser ModeCfg
brokerOpts = brokerFlag *> (BrokerCfg <$> myIP <*> myPort <*> schemaPath)


pubOpts :: Parser ModeCfg
pubOpts = pubFlag *> (PubCfg <$> brokerIP <*> brokerPort <*> schemaPath)


subOpts :: Parser ModeCfg
subOpts = subFlag *> (SubCfg <$>
  myIP <*> myPort <*> brokerIP <*> brokerPort <*> schemaPath <*> predicatesPath)


brokerFlag = switch (short 'b')
subFlag= switch (short 's')
pubFlag = switch (short 'p')

brokerIP = strOption (long "broker_ip" <> metavar "IP")
brokerPort = strOption (long "broker_port" <> metavar "PORT")
myIP = strOption (long "my_ip" <> metavar "IP")
myPort = strOption (long "my_port" <> metavar "PORT")
schemaPath = strOption (long "schema" <> metavar "SCHEMA_FILE")
predicatesPath = strOption (long "predicates" <> metavar "PREDS_FILE")


data ModeCfg =
    BrokerCfg {
      _brokerIP :: HostName
    , _brokerPort :: ServiceName
    , _brokerSchemaPath :: FilePath
    }
  | PubCfg {
      _pubBrokerIP :: HostName
    , _pubBrokerPort :: ServiceName
    , _pubSchemaPath :: FilePath
    }
  | SubCfg {
      _subIP :: HostName
    , _subPort :: ServiceName
    , _subBrokerIP :: HostName
    , _subBrokerPort :: ServiceName
    , _subSchemaPath :: FilePath
    , _subPredsPath:: FilePath
    }
  deriving (Show, Eq, Ord)
