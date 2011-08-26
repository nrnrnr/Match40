module Pages.Components (dateStr) where

import System.Locale (defaultTimeLocale)
import System.Time (ClockTime(..), formatCalendarTime, toUTCTime)

dateStr :: ClockTime -> String
dateStr ct =
  formatCalendarTime
    defaultTimeLocale
    "%a, %B %d, %Y at %H:%M:%S (UTC)"
    (toUTCTime ct)