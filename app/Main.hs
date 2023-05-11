{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Scotty (scotty, get, param, html)
import Lucid.Html5 (div_, class_, body_, link_, rel_, href_)
import Lucid (Html, renderText, toHtml)
import Data.Time.Calendar
import Data.Foldable (fold)
import Web.Scotty.Trans (middleware)
import Network.Wai.Middleware.Static (staticPolicy, noDots, (>->), addBase)


lastMonday :: Day
lastMonday = fromGregorian 2023 8 5

yearInWeeks :: [Day]
yearInWeeks = [lastMonday .. addDays 371 lastMonday]

printDay :: Day -> Html ()
printDay day = div_ [class_ "punchcard-day"] $ toHtml ("" :: String)

weekDaysHeaders :: Html()
weekDaysHeaders =
  div_ [class_ "week"]
    (fold $ map div_ ["Ma", "Ti", "On", "To", "Fr", "Lø", "Sø"])

splitPeriodsIntoWeeks :: [Html ()] -> Html()
splitPeriodsIntoWeeks periods =
  case periods of
    [] -> div_ [class_ "week"] ""
    p -> let week = (div_ [class_ "week"] $ fold (take 7 p)) in
          week <> fold [splitPeriodsIntoWeeks (drop 7 p)]

renderStyles :: Html ()
renderStyles = link_ [rel_ "stylesheet", href_ "styles.css" ]

renderPunchCard :: Html ()
renderPunchCard =
  let periods = map printDay yearInWeeks in
  -- renderText (div_ [class_ "punchcard"] (fold periods))
  body_  (div_ [class_ "punchcard"] (weekDaysHeaders <> splitPeriodsIntoWeeks periods))

main :: IO ()
main = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "static")
  get "/hello/:hello" $ do
      hello <- param "hello"
      html $ mconcat ["<h1>", hello, "</h1>"]
  get "/" $ do
      html $ renderText (renderPunchCard <> renderStyles)
