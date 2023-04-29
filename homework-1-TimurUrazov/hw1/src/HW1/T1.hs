{-# LANGUAGE LambdaCase #-}

module HW1.T1
  ( Day(..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import GHC.Natural (Natural)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -> Day
nextDay = \case
  Monday    -> Tuesday
  Tuesday   -> Wednesday
  Wednesday -> Thursday
  Thursday  -> Friday
  Friday    -> Saturday
  Saturday  -> Sunday
  Sunday    -> Monday

-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -> Day -> Day
afterDays after afterDay = nextDays afterDay $ after
  where
    nextDays day remaining = case remaining of
      0 -> day
      _ -> nextDays (nextDay day) $ remaining - 1

-- | Checks if the day is on the weekend.
isWeekend :: Day -> Bool
isWeekend = \case
  Saturday -> True
  Sunday   -> True
  _        -> False

-- | Computes the number of days until Friday.
daysToParty :: Day -> Natural
daysToParty Friday = 0
daysToParty day    = (+ 1) . daysToParty . nextDay $ day
