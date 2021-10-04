module FpsampleHaskell.App
    ( app
    ) where

import FpsampleHaskell.Monad
    ( MonadApp
    , MonadDataReader(..)
    , MonadDataWriter(..)
    )

app :: (MonadApp m) => m ()
app = readData >>= writeData
