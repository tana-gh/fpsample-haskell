module FpsampleHaskell.Monad
    ( MonadDataReader(readData)
    , MonadDataWriter(writeData)
    , MonadApp
    ) where

import FpsampleHaskell.Data
    ( Data
    )

class Monad m => MonadDataReader m where
    readData :: m Data

class Monad m => MonadDataWriter m where
    writeData :: Data -> m ()

class (MonadDataReader m, MonadDataWriter m) => MonadApp m
