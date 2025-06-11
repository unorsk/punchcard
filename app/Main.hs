module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DBClient
  ( addNewGroup
  , addOrUpdateToday
  , dbConnection
  , getUserId
  , initDB
  , retrieveGroupName
  , retrieveGroupsForUser
  , retrievePeriods
  )
import Model (DayDataPointGroup(..))

import Lucid (Html, form_, renderText, toHtml)
import Data.Pool (Pool, defaultPoolConfig, newPool, withResource)
import Data.Text.Lazy qualified as L
import Data.Time (getCurrentTime)
import Data.Time.Calendar
import Data.Time.Clock (utctDay)
import Database.SQLite.Simple (Connection, close)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import UI (pageAddGroup, pageGroup, pageIndex, renderPunchCardHtml)
import Web.Scotty (ActionM, get, html, post, scotty)
import Web.Scotty.Trans (captureParam, formParam, middleware, redirect)

initPool :: IO (Pool Connection)
initPool =
  newPool $ defaultPoolConfig dbConnection close 60.0 10

getRenderGroup' :: Day -> Int -> Int -> Pool Connection -> ActionM ()
getRenderGroup' today groupId userId pool = do
  (liftIO $ getRenderGroup today groupId userId pool) >>= html

getRenderGroup :: Day -> Int -> Int -> Pool Connection -> IO L.Text
getRenderGroup today groupId userId pool = do
  (groups, groupName, periods) <- liftIO $ withResource pool $ \c -> do
      groups <- retrieveGroupsForUser userId c
      groupName <- retrieveGroupName c groupId userId
      periods <- retrievePeriods c groupId userId
      pure (groups, groupName, periods)
  -- _ <- liftIO $ print days
  return $ pageGroup today groupId groups groupName periods

getRenderGroupHtml :: Day -> Int -> Int -> Pool Connection -> IO (Html ()) 
getRenderGroupHtml today groupId userId pool = do
  periods <- liftIO $ withResource pool $ \c -> do
    periods <- retrievePeriods c groupId userId
    pure periods
  -- _ <- liftIO $ print days
  return $ renderPunchCardHtml today groupId periods

main :: IO ()
main = do
  pool <- initPool
  -- withResource pool $ initDB "./db.sql"
  userId <- getUserId
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    -- get "/hello/:hello" $ do
    --   hello <- captureParam "hello"
    --   html $ mconcat ["<h1>", hello, "</h1>"]
    post "/add_group" $ do
      groupName :: String <- formParam "name"
      liftIO $ withResource pool $ addNewGroup groupName userId
      redirect "/"
    post "/:groupId" $ do
      groupId :: Int <- captureParam "groupId"
      liftIO $ withResource pool $ addOrUpdateToday groupId userId
      redirect ("/" <> L.pack (show groupId))
    get "/" $ do
      groups <- liftIO $ withResource pool $ retrieveGroupsForUser userId
      today <- liftIO getCurrentTime
      renderedGroups <- liftIO $ mapM (\DayDataPointGroup{id = groupIdAlias} -> getRenderGroupHtml (utctDay today) groupIdAlias userId pool) groups
      html $ pageIndex groups (foldl' (<>) mempty renderedGroups)
    get "/add_group" $ do
      html pageAddGroup
    get "/:groupId" $ do
      groupId :: Int <- captureParam "groupId"
      today <- liftIO getCurrentTime
      getRenderGroup' (utctDay today) groupId userId pool
    get "/:groupId/:day" $ do
      groupId :: Int <- captureParam "groupId"
      day :: String <- captureParam "day"
      -- todo handle database exceptions!
      getRenderGroup' (read day :: Day) groupId userId pool
