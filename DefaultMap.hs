module DefaultMap where

import Data.Map as Map
import Control.Concurrent.MVar
import Control.Monad

data DefaultMap k v = DM (k -> IO v) (MVar (Map k v))

empty :: (k -> IO v) -> IO (DefaultMap k v)
empty setter = liftM (DM setter) $ newMVar Map.empty

lookup :: (Ord k) => DefaultMap k v -> k -> IO v
lookup (DM setter mapM) key = modifyMVar mapM $ \map -> do
  case Map.lookup key map of
    Just val -> return (map, val)
    Nothing -> do
      newVal <- setter key
      return (Map.insert key newVal map, newVal)
