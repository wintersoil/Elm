module Luhn exposing (valid, convertToIntList, removeZeros, doubleOdd)

convertToIntList : String -> List Int -> List Int
convertToIntList l1 result =
    if (String.length l1 > 0) then
        convertToIntList (String.slice 1 (String.length l1) l1) (List.append result [(Maybe.withDefault 0 (String.toInt (String.slice 0 1 l1)))])
    else
        result

doubleOdd : List Int -> List Int
doubleOdd l1 =
    if (((modBy 2 (List.length l1)) == 0) == True) then
        (List.indexedMap (\i x-> if ((modBy 2 i == 0) == True && 2 * x > 9) then ((2 * x) - 9) else if ((modBy 2 i == 0) == True) then (2 * x) else x) l1)
    else
        (List.indexedMap (\i x -> if ((modBy 2 i == 0) == False && 2 * x > 9) then ((2 * x) - 9) else if ((modBy 2 i == 0) == False) then (2 * x) else x) l1)

removeZeros : String -> String -> String
removeZeros s1 result =
    if (String.length s1 > 0 && ((String.slice 0 1 s1) == " ") == False) then
        removeZeros (String.slice 1 (String.length s1) s1) (result ++ String.slice 0 1 s1)
    else if (String.length s1 > 0 && ((String.slice 0 1 s1) == " ") == True) then
        removeZeros (String.slice 1 (String.length s1) s1) (result)
    else
        result

valid : String -> Bool
valid input =
    if ((List.length (List.filter (\x -> x == ' ' || Char.isDigit x) (String.toList input)) == String.length input) == False ) then
        False
    else if (String.length (String.trim input) <= 1) then
        False
    else if (modBy 10 (List.sum (doubleOdd (convertToIntList (String.join "" (List.map (String.fromChar) (List.filter (\x -> Char.isDigit x && (x == ' ') == False) (String.toList (removeZeros input ""))) )) []) )) == 0) then
        True
    else
        False
