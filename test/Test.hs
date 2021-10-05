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

data TestEvent
    = ReadDataCalled
    | WriteDataCalled
    deriving (Eq, Show)

newtype TestState = TestState [TestEvent]

instance Monad m => MonadDataReader (TestApp TestState m) where
    readData = do
        modify $ \(TestState events) -> TestState (ReadDataCalled : events)
        return Data { name = "foo", age = 1 }
        
instance Monad m => MonadDataWriter (TestApp TestState m) where
    writeData _ = modify $ \(TestState events) -> TestState (WriteDataCalled : events)

instance Monad m => MonadApp (TestApp TestState m)

spec :: Spec
spec =
    describe "app" $
        it "readData and writeData are called correctly" $ do
            (TestState events) <- (`execStateT` TestState []) $ runApp app
            events `shouldBe` [ WriteDataCalled, ReadDataCalled ]

main :: IO ()
main = hspec spec
