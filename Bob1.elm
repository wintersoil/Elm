module Bob exposing (hey)
import Html as Html exposing (Html)


hey : String -> String
hey remark =
    if(remark == String.toUpper remark && String.contains "?" remark && String.any Char.isAlpha remark == True) then
        "Calm down, I know what I'm doing!"
    else if(String.endsWith "?" (String.trim remark) && String.length remark > 1) then
        "Sure."
    else if(remark == String.toUpper remark && String.any Char.isAlpha remark == True) then
        "Whoa, chill out!"
    else if(remark == "Bob" || String.trim remark == "") then
        "Fine. Be that way!"
    else
        "Whatever."

main =
  Html.text (hey "Bob amazing")
