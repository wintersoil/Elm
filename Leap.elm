module Leap exposing (isLeapYear)
import Html

isLeapYear : Int -> Bool
isLeapYear year =
    if (modBy 400 year == 0) then
        True
    else if (modBy 100 year == 0) then
        False
    else if (modBy 4 year == 0) then
        True
    else
        False

main = Html.text (if (isLeapYear 2104) then "Yes" else "No")
