module Grains exposing (square)

doublePrevious : Int -> Int -> Int -> Maybe Int
doublePrevious ptr sum n=
    if (n <= 0 || n > 64) then
        Nothing
    else if(ptr < n) then
        doublePrevious (ptr+1) (sum * 2) n
    else
        Just sum

square : Int -> Maybe Int
square n =
    doublePrevious 1 1 n
