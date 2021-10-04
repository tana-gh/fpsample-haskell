{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Main where

import Control.Monad.IO.Class
    ( MonadIO(..)
    )
import Control.Monad.Reader
    ( MonadReader
    , MonadTrans
    , ReaderT(..)
    , asks
    , runReaderT
    )
import Data.Aeson
    ( FromJSON
    )
import Network.HTTP.Simple
    ( Response
    , parseRequest
    , getResponseBody
    , httpJSON
    )
import Options.Applicative
    ( (<**>)
    , execParser
    , fullDesc
    , header
    , help
    , helper 
    , info
    , long
    , progDesc
    , strOption
    )
import Options.Applicative.Types
    ( Parser
    )
import FpsampleHaskell.App
    ( app
    )
import FpsampleHaskell.Data
    ( Data(..)
    )
import FpsampleHaskell.Monad
    ( MonadApp
    , MonadDataReader(..)
    , MonadDataWriter(..)
    )

newtype App r m a = App
    { runApp :: ReaderT r m a
    } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadReader r
    , MonadIO
    )

newtype Config = Config
    { url :: String
    }

instance FromJSON Data

instance MonadIO m => MonadDataReader (App Config m) where
    readData = do
        url' <- asks url
        req  <- liftIO $ parseRequest url'
        res  <- liftIO $ httpJSON req
        return $ getResponseBody res
        
instance MonadIO m => MonadDataWriter (App Config m) where
    writeData data' =
        liftIO $ do
            putStrLn $ "name=" ++ name data'
            putStrLn $ "age=" ++ show (age data')

instance MonadIO m => MonadApp (App Config m)

newtype Options = Options
    { urlOption :: String
    }

parser :: Parser Options
parser = Options <$> strOption (long "url" <> help "Set url for reading data.")

main :: IO ()
main = do
    options <- execParser $ info (parser <**> helper) fullDesc
    let config = Config $ urlOption options
    (`runReaderT` config) $ runApp app
