{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Random ( RandomT, MonadRandom
               , nextRandom
               , runRandomT
               , runRandomTStd) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Cont

import qualified System.Random as R

newtype RandomT m a = Random { unRandom :: StateT R.StdGen m a }
                    deriving (Monad, MonadTrans, MonadState R.StdGen, MonadIO)

class Monad m => MonadRandom m where
  nextRandom :: R.Random a => m a

instance Monad m => MonadRandom (RandomT m) where
  nextRandom = do
    g <- get
    let (r, newG) = R.random g
    put newG
    return r

instance (MonadRandom m) => MonadRandom (StateT s m) where
  nextRandom = lift nextRandom

instance (MonadRandom m) => MonadRandom (ReaderT r m) where
  nextRandom = lift nextRandom

instance (MonadRandom m) => MonadRandom (ContT r m) where
  nextRandom = lift nextRandom

runRandomT :: Monad m => RandomT m a -> R.StdGen -> m a
runRandomT r gen = liftM fst $ runStateT (unRandom r) gen

runRandomTSeed :: Monad m => RandomT m a -> Int -> m a
runRandomTSeed r seed = liftM fst $ runStateT (unRandom r) (R.mkStdGen seed)

runRandomTStd :: MonadIO m => RandomT m a -> m a
runRandomTStd r = liftM fst (runStateT (unRandom r) =<< liftIO R.getStdGen)



