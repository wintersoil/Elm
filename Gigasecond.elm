module Gigasecond exposing (add)

import Time
import Html


add : Time.Posix -> Time.Posix
add timestamp =
  Time.millisToPosix (Time.posixToMillis timestamp + 1000000000000)

type Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec

toEnglishMonth: Month -> String
toEnglishMonth month =
  case month of
    Jan -> "January"
    Feb -> "February"
    Mar -> "March"
    Apr -> "April"
    May -> "May"
    Jun -> "June"
    Jul -> "July"
    Aug -> "August"
    Sep -> "September"
    Oct -> "October"
    Nov -> "November"
    Dec -> "December"

p: Time.Posix
p = Time.millisToPosix(1604766553000)

main =
  Html.text ( (String.fromInt (Time.toYear Time.utc (add (p))))
  ++ " / " ++ (String.fromInt (Time.toDay Time.utc (add (p))))
  ++ "     " ++ (String.fromInt (Time.toHour Time.utc (add (p))))
  ++ ":" ++ (String.fromInt (Time.toMinute Time.utc (add (p))))
  ++ ":" ++ (String.fromInt (Time.toSecond Time.utc (add (p)))))
