{-# LANGUAGE DeriveGeneric #-}

module Model (DayDataPoint (..), DayDataPointGroup (..)) where

import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

data DayDataPoint = DayDataPoint
  { count :: Int
  , groupId :: Int
  , date :: Day
  }
  deriving (Eq, Generic)

data DayDataPointGroup = DayDataPointGroup
  { id :: Int
  , name :: String
  }
  deriving (Eq, Generic)

instance Show DayDataPoint where
  show (DayDataPoint count _ date) = show count <> show date
