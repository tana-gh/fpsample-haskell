{-# LANGUAGE DeriveGeneric #-}

module FpsampleHaskell.Data
    ( Data(..)
    ) where

import GHC.Generics
    ( Generic
    )

data Data = Data
    { name :: String
    , age  :: Int
    } deriving Generic
