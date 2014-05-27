{-# LANGUAGE RankNTypes #-}
module Pipes.SQLite (
                     -- * Producers - turn a query into a stream of results
                     query,
                     query_,
                     queryNamed,
                     eachRow,
                     -- * Consumers - execute a statement using upstream values as bound parameters
                     execute,
                     executeNamed
                    ) where

import           Pipes as P
import           Pipes.Safe as PS
import qualified Database.SQLite.Simple as SQL
import           Control.Monad (forever)

-- | Run a parameterless query and yield each parsed row in the result.
query_ :: (MonadSafe m, SQL.FromRow a)
        => SQL.Connection
        -> SQL.Query
        -> Producer' a m ()
query_ conn q = bracketIO (SQL.openStatement conn q) SQL.closeStatement eachRow

-- | Run a query with parameters and yield each parsed row in the result.
query :: (MonadSafe m, SQL.FromRow a, SQL.ToRow params)
       => SQL.Connection
       -> SQL.Query
       -> params
       -> Producer' a m ()
query conn q p = bracketIO open' SQL.closeStatement eachRow
    where open' = do s <- SQL.openStatement conn q
                     SQL.bind s p
                     return s

-- | Run a query with named parameters and yield each parsed row in the result.
queryNamed :: (MonadSafe m, SQL.FromRow a)
            => SQL.Connection
            -> SQL.Query
            -> [SQL.NamedParam]
            -> Producer' a m ()
queryNamed conn q ns = bracketIO open' SQL.closeStatement eachRow
    where open' = do s <- SQL.openStatement conn q
                     SQL.bindNamed s ns
                     return s

-- | Executes the SQL statment by binding each value provided by each upstream yield.
execute :: (MonadIO m, SQL.ToRow q) => SQL.Connection -> SQL.Query -> Consumer' q m b
execute conn q =  writeEach
    where writeEach = forever $ do
            v <- await
            liftIO $ SQL.execute conn q v

-- | Executes the SQL statement with the set of bound parameters provided by each upstream yield.
executeNamed :: MonadIO m => SQL.Connection -> SQL.Query -> Consumer' [SQL.NamedParam]  m b
executeNamed conn q = forever $ do
                         v <- await
                         liftIO $ SQL.executeNamed conn q v

-- | Yields each row in the given Statement. It is assumed that the statement has already been opened.
eachRow :: (MonadIO m, SQL.FromRow a) => SQL.Statement -> Producer' a m ()
eachRow s = do
  v <- liftIO (SQL.nextRow s)
  case v of
    Just z -> yield z >> eachRow s
    Nothing -> return ()

-- | Bracket a MonadSafe action with IO.
bracketIO :: MonadSafe m => IO a -> (a -> IO a1) -> (a -> m c) -> m c
bracketIO a b = PS.bracket (liftIO a) (liftIO . b)
