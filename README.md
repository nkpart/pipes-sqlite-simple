pipes-sqlite-simple
===================

Functions that smash Pipes, Pipes.Safe and sqlite-simple together

Currently this projects depends on a change to sqlite-simple: https://github.com/nurpax/sqlite-simple/pull/39

Usage
---

    > import Pipes.SQLite
    
    > import qualified Pipes as P
    > import qualified Pipes.Prelude as P
    > import Pipes.Safe
    > import qualified Database.SQLite.Simple as SQL
    
    :set -XOverloadedStrings
    conn <- SQL.open "test.sqlite"
    SQL.execute_ conn "create table woozles (name)"
    -- Create woozles
    P.each [(1 :: Int)..10] >-> P.map SQL.Only >-> execute conn "insert into woozles (name) values (?)"
    -- Query the woozles.
    runSafeT . runEffect $ 
        query_ conn "select name from woozles" >-> (P.print :: Consumer (SQL.Only Int) (SafeT IO) ())
    
    
    
    
