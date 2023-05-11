{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where
import Web.Scotty (scotty, get, param, html)
import Lucid.Html5 (div_, class_, body_, link_, rel_, href_, button_, method_, )
import Lucid ( Html, renderText, toHtml, form_, head_ )
import Data.Time.Calendar
import Data.Foldable (fold)
import Web.Scotty.Trans (middleware)
import Network.Wai.Middleware.Static (staticPolicy, noDots, (>->), addBase)
import Data.Pool (Pool, newPool, defaultPoolConfig, withResource)
import Database.MySQL.Simple (Connection, ConnectInfo (connectHost, connectDatabase, connectUser, connectPassword), close, connect, defaultConnectInfo, query_)
import GHC.Generics
import Control.Monad.IO.Class (MonadIO(liftIO))

data DayDataPoint = DayDataPoint
  {
    count :: Int
    , date :: String
  }
  deriving (Eq, Generic)

lastMonday :: Day
lastMonday = fromGregorian 2023 8 5

yearInWeeks :: [Day]
yearInWeeks = [lastMonday .. addDays 370 lastMonday]

printDay :: Day -> Html ()
printDay day = div_ [class_ "punchcard-day"] $ toHtml ("" :: String)

weekDaysHeaders :: Html()
weekDaysHeaders =
  div_ [class_ "week" ]
    (fold $ map (div_ [class_ "weekday-title"]) ["Ma", "Ti", "On", "To", "Fr", "Lø", "Sø"])

splitPeriodsIntoWeeks :: [Html ()] -> Html()
splitPeriodsIntoWeeks periods =
  let week = (div_ [class_ "week"] $ fold (take 7 periods))
      rest = drop 7 periods in
    week <> fold (if length rest > 0 then [splitPeriodsIntoWeeks rest] else [])

renderStyles :: Html ()
renderStyles = link_ [rel_ "stylesheet", href_ "styles.css" ]

renderPunchButtons :: Html ()
renderPunchButtons =
  form_ [method_ "POST"] (button_ "PUNCH!")

renderPunchCard :: Html ()
renderPunchCard =
  let periods = map printDay yearInWeeks in
  -- renderText (div_ [class_ "punchcard"] (fold periods))
  div_ [class_ "punchcard"] (weekDaysHeaders <> splitPeriodsIntoWeeks periods)

initPool :: IO (Pool Connection)
initPool = newPool $ defaultPoolConfig doConnect close 60.0 10

doConnect :: IO Connection
doConnect = connect defaultConnectInfo {connectHost = "aws-eu-west-2.connect.psdb.cloud", connectPassword = "pscale_pw_1Enrrcl6Ao6ZZHYmN37pNcvDeJQtURzNhyfkFnVbh30", connectUser = "nzlmcpp2w4p8mrdfjz7v", connectDatabase = "harpa"}

retrievePeriods :: Connection -> IO [DayDataPoint]
retrievePeriods conn = do
  r <- query_ conn "select * from datapoints"
  return $ map (uncurry DayDataPoint) r

main :: IO ()
main = do
  pool <- initPool
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    get "/hello/:hello" $ do
        hello <- param "hello"
        html $ mconcat ["<h1>", hello, "</h1>"]
    get "/" $ do
        -- todo handle exceptions!
        -- preiods <- liftIO $ withResource pool retrievePeriods
        -- combine empty periods + periods from database that have some data
        -- and display the punch card
        html $ renderText (head_ renderStyles
          <> body_ renderPunchCard
            <> renderPunchButtons)
