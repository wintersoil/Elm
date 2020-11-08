module CollatzConjecture exposing (collatz)
import Html

callReducer : Int -> Int -> Int
callReducer start counter =
    if(start == 1) then
        counter
    else if(modBy 2 start == 0) then
        callReducer (start // 2) (counter + 1)
    else
        callReducer ((start * 3) + 1) (counter + 1)

collatz : Int -> Result String Int
collatz start =
    if(start <= 0) then
        Result.Err "Only positive numbers are allowed"
    else
        Result.Ok (callReducer start 0)

main = Html.text (String.fromInt (Result.withDefault 0 (collatz 1000000)) )
