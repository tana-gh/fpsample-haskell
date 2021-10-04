{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Main where

import Control.Monad.State
    ( MonadIO
    , MonadState
    , MonadTrans
    , StateT(..)
    , execStateT
    , modify
    )
import Test.Hspec
    ( Spec
    , hspec
    , describe
    , it
    , shouldBe
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

newtype TestApp s m a = TestApp
    { runApp :: StateT s m a
    } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadState s
    , MonadIO
    )

data TestState = TestState
    { readDataCalled  :: Bool
    , writeDataCalled :: Bool
    }

instance Monad m => MonadDataReader (TestApp TestState m) where
    readData = do
        modify $ \s -> s { readDataCalled = True }
        return Data { name = "foo", age = 1 }
        
instance Monad m => MonadDataWriter (TestApp TestState m) where
    writeData _ = modify $ \s -> s { writeDataCalled = True }

instance Monad m => MonadApp (TestApp TestState m)

spec :: Spec
spec =
    describe "app" $
        it "readData and writeData are called correctly" $ do
            st <- (`execStateT` TestState False False) $ runApp app
            readDataCalled  st `shouldBe` True
            writeDataCalled st `shouldBe` True

main :: IO ()
main = hspec spec
