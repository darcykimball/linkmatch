module Main where


import Data.Monoid ((<>))
import Network.Simple.TCP
import Options.Applicative

import Broker
import Subscriber
import Publisher
import Event


main :: IO ()
main = do
  modeCfg <- execParser opts
  print modeCfg    
  where
    opts = info (brokerOpts <|> pubOpts <|> subOpts)
      (fullDesc <> progDesc "A pub/sub node daemon.")

--
-- Command-line parsing stuff
-- 



brokerOpts :: Parser ModeCfg
brokerOpts = brokerFlag *> (BrokerCfg <$> myIP <*> myPort)


pubOpts :: Parser ModeCfg
pubOpts = pubFlag *> (PubCfg <$> myIP <*> brokerIP <*> brokerPort <*> schemaPath)


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
    }
  | PubCfg {
      _pubIP :: HostName
    , _pubBrokerIP :: HostName
    , _pubBrokerPort :: ServiceName
    , _pubSchemaPath :: FilePath
    }
  | SubCfg {
      _subIP :: HostName
    , _subPort :: ServiceName
    , _subBrokerIP :: HostName
    , _subBrokerPort :: ServiceName
    , _subSchemaPath :: FilePath
    , _subPredicatesPath :: FilePath
    }
  deriving (Show, Eq, Ord)
