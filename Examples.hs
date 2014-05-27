{-# LANGUAGE OverloadedStrings #-}
module Examples where

import           Control.Applicative ((<$>))
import qualified Database.SQLite.Simple as SQL
import           Pipes as P
import           Pipes.Prelude as P
import           Pipes.Safe
import           Pipes.SQLite

-- EXAMPLE TIME
newtype Widget = Widget Int deriving Show

instance SQL.FromRow Widget where
    fromRow = Widget <$> SQL.field

printSum :: Widget -> IO ()
printSum = Prelude.print

main :: IO ()
main = do
  conn <- SQL.open "example.sqlite"
  _ <- try $ SQL.execute_ conn (SQL.Query "DROP TABLE wats") :: IO (Either SomeException ())
  SQL.execute_ conn (SQL.Query "CREATE TABLE wats (wat INTEGER)")
  runSafeT . runEffect $ do
    (query_ conn "select 1 + 1" >-> P.take 5) `for` (liftIO . printSum)
    (query conn "select 1 + ?" (SQL.Only (5::Int)) >-> P.take 5) `for` (liftIO . printSum)
    each [(1::Int)..] >-> P.map SQL.Only >-> P.take 5 >-> execute conn "INSERT INTO wats (wat) VALUES (?)"
    liftIO . putStrLn $ "Checking those wats"
    runEffect $ query_ conn "select wat from wats" `for` (liftIO . printSum)
  SQL.close conn
