{-# LANGUAGE OverloadedRecordDot #-}

module UI (pageGroup, pageIndex, pageAddGroup) where

import Model (DayDataPointGroup(..), DayDataPoint(..))
import Data.Time.Calendar
import Data.Text (pack)
import Lucid.Html5 (h2_, div_, class_, link_, rel_, href_, button_, method_, title_, a_, action_, head_, body_, input_, type_, name_, )
import Lucid ( Html, toHtml, form_, renderText )
import Data.Foldable (fold)
import Data.Text.Lazy (Text)
import Data.Char (toUpper)
import Data.List (find)
import Data.Maybe (fromMaybe)

pageIndex :: [DayDataPointGroup] -> Text
pageIndex groups =
  renderText
          ( head_ renderStyles
              <> body_ (renderGroups groups (-1))
          )

pageAddGroup :: Text
pageAddGroup =
  renderText
          ( head_ renderStyles
              <> body_
                ( form_
                    [method_ "POST", name_ "group", action_ "/add_group"]
                    (input_ [type_ "text", name_ "name"] <> button_ [action_ "submit"] "ADD")
                )
          )


-- TODO (:
lastMonday :: Day
lastMonday = fromGregorian 2024 12 16

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
    a_ [href_ $ pack ("/" <> show day.groupId <> "/" <> show day.date), class_ ("punchcard-day " <> pack (dayClass day.count) <> todayClass), title_ (pack $ (show day.date) <> " " <> show day.count)] $ toHtml ("" :: String)

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
renderStyles = link_ [rel_ "stylesheet", href_ "/styles.css" ]

renderGroup :: DayDataPointGroup -> Bool -> Html ()
renderGroup g isActive =
  let isActiveClass = if isActive then "active_group" else "" in
    a_ [href_ (pack ("/" <> show g.id)), class_ isActiveClass] (toHtml g.name)

addGroupLink :: Html ()
addGroupLink =
  a_ [href_ "/add_group"] "+ Add"

renderGroups :: [DayDataPointGroup] -> Int -> Html ()
renderGroups groups currentGroupId =
  div_ [class_ "groups_links"] (mconcat (map (\g -> renderGroup g (g.id == currentGroupId)) groups) <> addGroupLink)

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

pageGroup :: Day -> Int -> [DayDataPointGroup] -> String -> [DayDataPoint] -> Text
pageGroup today groupId  groups groupName periods = 
  let days = map (\y ->
          let count = find (\d -> d.date == y) periods in
            fromMaybe DayDataPoint {count = 0, groupId = groupId, date = y} count) yearInWeeks in do
      -- _ <- liftIO $ print days
      renderText (head_ renderStyles <> title_ (toHtml $ "👊 " <> map toUpper groupName)
        <> body_ (renderGroups groups groupId)
          <> renderPunchCardHeader groupName
          <> renderPunchCard days today
          <> renderPunchButtons groupId)