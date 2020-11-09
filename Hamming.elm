module Hamming exposing (distance)

compareToOther : List Char -> Int -> List Char -> Result String Int
compareToOther right count left=
    if((List.length right) == (List.length left) && List.length right >= 1 && ((List.head left) == (List.head right)) == False) then
        compareToOther (List.drop 1 (right)) (count + 1) (List.drop 1 (left))
    else if((List.length right) == (List.length left) && List.length right >= 1 && (List.head left) == (List.head right)) then
            compareToOther (List.drop 1 (right)) (count) (List.drop 1 (left))
    else if(((List.length right) == (List.length left)) == False) then
        Result.Err "left and right strands must be of equal length"
    else
        Result.Ok count

distance : String -> String -> Result String Int
distance left right =
    compareToOther (String.toList right) 0 (String.toList left)
