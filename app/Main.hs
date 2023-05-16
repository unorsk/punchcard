{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where
import Web.Scotty (scotty, get, post, param, html)
import Lucid.Html5 (h2_, div_, class_, body_, link_, rel_, href_, button_, method_, title_, a_, action_, )
import Lucid ( Html, renderText, toHtml, form_, head_ )
import Data.Time.Calendar
import Data.Foldable (fold)
import Web.Scotty.Trans (middleware, redirect)
import Network.Wai.Middleware.Static (staticPolicy, noDots, (>->), addBase)
import Data.Pool (Pool, newPool, defaultPoolConfig, withResource)
import Database.MySQL.Simple
import GHC.Generics
import Control.Monad.IO.Class (MonadIO(liftIO))
import Dhall (FromDhall, auto, input)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime)
import Data.Time.Clock (utctDay)
import Data.Text (pack)
import qualified Data.Text.Lazy as L

data MyConnectInfo = MyConnectInfo
  {
    host :: String
    , user :: String
    , password :: String
    , database :: String
  } deriving Generic

instance FromDhall MyConnectInfo


data DayDataPoint = DayDataPoint
  {
    count :: Int
    , date :: Day
  }
  deriving (Eq, Generic)

data DayDataPointGroup = DayDataPointGroup
  {
    id :: Int
    , name :: String
  }
  deriving (Eq, Generic)

instance Show DayDataPoint where
  show (DayDataPoint count date) = (show count) <> (show date)

lastMonday :: Day
lastMonday = fromGregorian 2023 5 1

yearInWeeks :: [Day]
yearInWeeks = [lastMonday .. addDays 370 lastMonday]

dayClass :: Int -> String
dayClass 0 = ""
dayClass 1 = "day-contributions-1"
dayClass 2 = "day-contributions-2"
dayClass 3 = "day-contributions-3"
dayClass _ = "day-contributions-4"

printDay :: Day -> DayDataPoint -> Html ()
printDay today day =
  -- day-contributions-
  let todayClass = if day.date == today then " today" else "" in
    div_ [class_ ("punchcard-day " <> pack (dayClass day.count) <> todayClass), title_ (pack $ (show day.date) <> " " <> show day.count)] $ toHtml ("" :: String)

weekDaysHeaders :: Html()
weekDaysHeaders =
  div_ [class_ "week" ]
    (Data.Foldable.fold $ map (div_ [class_ "weekday-title"]) ["Ma", "Ti", "On", "To", "Fr", "Lø", "Sø"])

splitPeriodsIntoWeeks :: [Html ()] -> Html()
splitPeriodsIntoWeeks periods =
  let week = (div_ [class_ "week"] $ Data.Foldable.fold (take 7 periods))
      rest = drop 7 periods in
    week <> Data.Foldable.fold (if length rest > 0 then [splitPeriodsIntoWeeks rest] else [])

renderStyles :: Html ()
renderStyles = link_ [rel_ "stylesheet", href_ "styles.css" ]

renderGroup :: DayDataPointGroup -> Bool -> Html ()
renderGroup g isActive =
  let isActiveClass = if isActive then "active_group" else "" in
    a_ [href_ (pack ("/" <> show g.id)), class_ isActiveClass] (toHtml g.name)

renderGroups :: [DayDataPointGroup] -> Int -> Html ()
renderGroups groups currentGroupId =
  div_ [class_ "groups_links"] (mconcat (map (\g -> renderGroup g (g.id == currentGroupId)) groups))

renderPunchCardHeader :: String -> Html ()
renderPunchCardHeader groupName =
  -- a_ [href_ "/"] "<" <>
  h2_ (toHtml groupName)

renderPunchButtons :: Int -> Html ()
renderPunchButtons groupId =
  form_ [method_ "POST", action_ ("/" <> pack (show groupId))] (button_ "PUNCH IT!")

renderPunchCard :: [DayDataPoint] -> Day -> Html ()
renderPunchCard days today =
  let periods = map (printDay today) days in
  -- renderText (div_ [class_ "punchcard"] (fold periods))
  div_ [class_ "punchcard"] (weekDaysHeaders <> splitPeriodsIntoWeeks periods)

initPool :: MyConnectInfo -> IO (Pool Connection)
initPool connInfo =
  -- trying to fix the ssl error
  -- let mysqlConnInfo = ConnectInfo {
  --   connectHost = connInfo.host
  --   , connectPort = 3306
  --   , connectSSL = Nothing
  --   , connectUser = connInfo.user
  --   , connectOptions = []
  --   , connectPath = "sslaccept=strict"
  --   , connectPassword = connInfo.password
  --   , connectDatabase = connInfo.database} in
  let mysqlConnInfo = defaultConnectInfo {
    connectHost = connInfo.host
    , connectUser = connInfo.user
    , connectPassword = connInfo.password
    , connectDatabase = connInfo.database} in
  newPool $ defaultPoolConfig (connect mysqlConnInfo) close 60.0 10

addOrUpdateToday :: Connection -> Int -> Int -> IO ()
addOrUpdateToday conn groupId userId = do
  r <- query conn "select `id`, `count` from `datapoints` where `date` = CURRENT_DATE() and `group_id` = ? and `user_id` = ?" [groupId, userId]
  case length r of
    0 -> do
      _ <- execute conn "insert into `datapoints` (`date`, `group_id`, `user_id`) values (CURRENT_DATE(), ?, ?)" [groupId, userId]
      return ()
    _ -> mapM_ (\(id_::Int, _::Int) -> do
          _ <- execute conn "update `datapoints` set `count` = `count`+1 where `id` = ?" $ Only id_
          return ()
        ) r

retrieveGroupName :: Connection -> Int -> Int -> IO String
retrieveGroupName conn groupId userId = do
  r::[(Int, String)] <- query conn "select `id`, `name` from datapoints_groups where `id` = ? and `user_id` = ?" [groupId, userId]
  return (mconcat $ map (\(_, s) -> s) r)

retrieveGroupsForUser :: Connection -> Int -> IO [DayDataPointGroup]
retrieveGroupsForUser conn userId = do
  r <- query conn "select `id`, `name` from datapoints_groups where `user_id` = ?" $ Only userId
  return $ map (uncurry DayDataPointGroup) r

retrievePeriods :: Connection -> Int -> Int -> IO [DayDataPoint]
retrievePeriods conn groupId userId = do
  -- r <- query_ conn "select `count`, CAST(`date` as CHAR(50)) from datapoints"
  r <- query conn "select `count`, `date` from datapoints where `user_id` = ? and `group_id` = ?" [userId, groupId]
  return $ map (uncurry DayDataPoint) r

getUserId :: IO Int
getUserId = pure 1

main :: IO ()
main = do
  connInfo :: MyConnectInfo <- input auto "./local.dhall"
  pool <- initPool connInfo
  userId <- getUserId
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    get "/hello/:hello" $ do
        hello <- param "hello"
        html $ mconcat ["<h1>", hello, "</h1>"]
    post "/:groupId" $ do
        groupId::Int <- param "groupId"
        liftIO $ withResource pool (\c -> addOrUpdateToday c groupId userId)
        redirect ("/" <> L.pack (show groupId))
    get "/" $ do
      groups <- liftIO $ withResource pool (\c -> retrieveGroupsForUser c userId)
      html $ renderText (head_ renderStyles
              <> body_ (renderGroups groups (-1)))
    get "/:groupId" $ do
        groupId::Int <- param "groupId"
        -- todo handle database exceptions!
        groups <- liftIO $ withResource pool (\c -> retrieveGroupsForUser c userId)
        groupName <- liftIO $ withResource pool (\c -> retrieveGroupName c groupId userId)
        periods <- liftIO $ withResource pool (\c -> retrievePeriods c groupId userId)
        today <- liftIO getCurrentTime
        let days = map (\y ->
                let count = find (\d -> d.date == y) periods in
                  fromMaybe DayDataPoint {count = 0, date = y} count) yearInWeeks in do
            -- _ <- liftIO $ print days
            html $ renderText (head_ renderStyles
              <> body_ (renderGroups groups groupId)
                <> renderPunchCardHeader groupName
                <> renderPunchCard days (utctDay today)
                <> renderPunchButtons groupId)
