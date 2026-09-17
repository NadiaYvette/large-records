{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

-- | Reader-soup monad transformer over anonymous records (inspired by porcupine's record-soup)
module Data.Record.Anon.Soup (
    -- * Soup monad and transformer
    SoupT(..)
  , Soup
  , runSoup
    -- * Core operations
  , askSoup
  , askField
  , askSubSoup
  , zoomSoup
  , pour
  , pourRecord
  , scoop
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Functor.Identity (Identity(..))

import Data.Record.Anon (Field, RowHasField, SubRow)
import qualified Data.Record.Anon.Simple as S

-- | Reader-soup monad transformer parameterized by an anonymous record row @r@
newtype SoupT r m a = SoupT { runSoupT :: S.Record r -> m a }

-- | Pure soup monad over an anonymous record
type Soup r a = SoupT r Identity a

-- | Run a pure soup action
runSoup :: Soup r a -> S.Record r -> a
runSoup m = runIdentity . runSoupT m

instance Functor m => Functor (SoupT r m) where
  fmap f (SoupT act) = SoupT (fmap f . act)

instance Applicative m => Applicative (SoupT r m) where
  pure = SoupT . const . pure
  SoupT mf <*> SoupT mx = SoupT (\r -> mf r <*> mx r)

instance Monad m => Monad (SoupT r m) where
  return = pure
  SoupT act >>= f = SoupT (\r -> act r >>= \x -> runSoupT (f x) r)

instance MonadTrans (SoupT r) where
  lift = SoupT . const

instance MonadIO m => MonadIO (SoupT r m) where
  liftIO = lift . liftIO

instance Monad m => MonadReader (S.Record r) (SoupT r m) where
  ask = SoupT return
  local f (SoupT act) = SoupT (act . f)

-- | Read the full environment record
askSoup :: Monad m => SoupT r m (S.Record r)
askSoup = ask

-- | Read a specific field from the environment record
askField :: forall n r a m. (Monad m, RowHasField n r a) => Field n -> SoupT r m a
askField fld = SoupT (return . S.get fld)

-- | Read a sub-record from the environment record
askSubSoup :: forall r r' m. (Monad m, SubRow r r') => SoupT r m (S.Record r')
askSubSoup = SoupT (return . S.project)

-- | Zoom a soup action: an action requiring only a sub-row @small@ can run inside an environment providing a super-row @big@
zoomSoup :: forall big small m a. SubRow big small => SoupT small m a -> SoupT big m a
zoomSoup (SoupT act) = SoupT (act . S.project)

-- | Pour an available environment record into a soup action that expects a sub-row of it
pour :: forall big small m a. SubRow big small => SoupT small m a -> S.Record big -> m a
pour act env = runSoupT act (S.project env)

-- | Flipped version of 'pour'
pourRecord :: forall big small m a. SubRow big small => S.Record big -> SoupT small m a -> m a
pourRecord env act = pour act env

-- | Construct a soup action from a function on the environment record
scoop :: (S.Record r -> m a) -> SoupT r m a
scoop = SoupT
