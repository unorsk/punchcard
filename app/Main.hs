{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Scotty (scotty, get, param, html)
import Lucid.Html5 (div_, class_)
import Lucid (Html, renderText, toHtml)
import Data.Text.Internal.Lazy (Text)
import Data.Time.Calendar


lastMonday :: Day
lastMonday = fromGregorian 2023 8 5

yearInWeeks :: [Day]
yearInWeeks = [lastMonday .. addDays 371 lastMonday]

printDay :: Day -> Html ()
printDay day = div_ [class_ "punchcard-day"] $ toHtml $ show day


renderPunchCard :: Text
renderPunchCard = 
  let periods = map printDay yearInWeeks in
  renderText (div_ [class_ "punchcard"] (foldr (<>) (div_ "") periods))

main :: IO ()
main = scotty 3000 $ do
  get "/hello/:hello" $ do
      hello <- param "hello"
      html $ mconcat ["<h1>", hello, "</h1>"]
  get "/" $ do
      html renderPunchCard
