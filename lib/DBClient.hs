module DBClient
  ( initDB
  , retrieveGroupName
  , retrieveGroupsForUser
  , retrievePeriods
  , addNewGroup
  , addOrUpdateToday
  , getUserId
  , dbConnection
  ) where

import Data.Time.Calendar (Day)
import Database.SQLite.Simple
  ( Connection
  , NamedParam ((:=))
  , close
  , executeNamed
  , execute_
  , open
  , queryNamed
  )
import Model (DayDataPoint (..), DayDataPointGroup (..))

initDB :: String -> Connection -> IO ()
initDB dbSqlInitFile c = do
  initSql <- readFile dbSqlInitFile
  execute_ c $ read initSql

dbConnection :: IO Connection
dbConnection = open "./db.sqlite"

retrievePeriods :: Connection -> Int -> Int -> IO [DayDataPoint]
retrievePeriods conn groupId userId =
  map (\(count :: Int, day :: Day) -> DayDataPoint count groupId day)
    <$> queryNamed
      conn
      "select `count`, `date` from datapoints where `user_id` = :userId and `group_id` = :groupId"
      [":userId" := userId, ":groupId" := groupId]

addOrUpdateToday :: Int -> Int -> Connection -> IO ()
addOrUpdateToday groupId userId conn = do
  r <-
    queryNamed
      conn
      "select `id`, `count` from `datapoints` where `date` = DATE() and `group_id` = :groupId and `user_id` = :userId"
      [":groupId" := groupId, ":userId" := userId]
  case length r of
    0 -> do
      _ <-
        executeNamed
          conn
          "insert into `datapoints` (`date`, `group_id`, `user_id`, `count`) values (DATE(), :groupId, :userId, 1)"
          [":groupId" := groupId, ":userId" := userId]
      return ()
    _ ->
      mapM_
        ( \(id_ :: Int, _ :: Int) -> do
            _ <-
              executeNamed
                conn
                "update `datapoints` set `count` = `count`+1 where `id` = :id"
                [":id" := id_]
            return ()
        )
        r

retrieveGroupName :: Connection -> Int -> Int -> IO String
retrieveGroupName conn groupId userId = do
  r :: [(Int, String)] <-
    queryNamed
      conn
      "select `id`, `name` from datapoints_groups where `id` = :groupId and `user_id` = :userId"
      [":groupId" := groupId, ":userId" := userId]
  return (mconcat $ map (\(_, s) -> s) r)

addNewGroup :: String -> Int -> Connection -> IO ()
addNewGroup groupName userId conn = do
  _ <-
    executeNamed
      conn
      "insert into `datapoints_groups` (`name`, `user_id`) values (:groupName, :userId)"
      [":groupName" := groupName, ":userId" := userId]
  return ()

retrieveGroupsForUser :: Int -> Connection -> IO [DayDataPointGroup]
retrieveGroupsForUser userId conn = do
  r <-
    queryNamed
      conn
      "select `id`, `name` from datapoints_groups where `user_id` = :userId"
      [":userId" := userId]
  return $ map (uncurry DayDataPointGroup) r

getUserId :: IO Int
getUserId = pure 1
