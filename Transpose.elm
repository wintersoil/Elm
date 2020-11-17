module Transpose exposing (transpose, gatherNthCol, clipListStringsByOne, maxLength, paddingSpaces)

maxLength: List String -> Int -> Int
maxLength l1 maxl =
    if (List.length l1 > 0 && maxl <= String.length (Maybe.withDefault "" (List.head l1))) then
        maxLength (List.drop 1 l1) (String.length (Maybe.withDefault "" (List.head l1)))
    else if (List.length l1 > 0 && maxl > String.length (Maybe.withDefault "" (List.head l1))) then
        maxLength (List.drop 1 l1) maxl
    else
        maxl

gatherNthCol: List String -> List String -> Int -> List String
gatherNthCol l1 result ptr =
    if (maxLength l1 0 > 0) then
        gatherNthCol ((clipListStringsByOne l1 [])) (List.append result [(gatherFirstStripe l1 "")]) (ptr + 1)
    else
        result

gatherFirstStripe : List String -> String -> String
gatherFirstStripe l1 result =
    if (List.length l1 > 0) then
        gatherFirstStripe (List.drop 1 l1) (String.append result (String.slice 0 1 (Maybe.withDefault "_" (List.head l1)) ))
    else
        result

clipListStringsByOne: List String -> List String -> List String
clipListStringsByOne l1 result =
    if (List.length l1 > 0) then
        clipListStringsByOne (List.drop 1 l1) (List.append result [String.slice 1 (String.length (Maybe.withDefault "z" (List.head l1))) (Maybe.withDefault "z" (List.head l1))])
    else
        result

paddingSpaces: List String -> List String -> Int -> List String
paddingSpaces l1 result lengthy=
    if (List.length l1 > 0 && ((String.length (Maybe.withDefault "c" (List.head l1))) < (maxLength l1 0) )) then
        paddingSpaces (List.drop 1 l1)  (List.append result [(String.append (String.slice 0 (String.length (Maybe.withDefault "z" (List.head l1))) (Maybe.withDefault "z" (List.head l1))) (String.concat (List.repeat (((maxLength l1 0)) - (String.length (Maybe.withDefault "c" (List.head l1)))) " " )))] ) (maxLength l1 0)
    else if (List.length l1 > 0 && ((String.length (Maybe.withDefault "c" (List.head l1))) == (maxLength l1 0))) then
        paddingSpaces (List.drop 1 l1) (List.append result ([String.slice 0 (String.length (Maybe.withDefault "z" (List.head l1))) (Maybe.withDefault "z" (List.head l1))])) (maxLength l1 0)
    else
        result

transposeHelper: List String -> List String -> Int -> List String
transposeHelper l1 result ptr=
    (gatherNthCol l1 result ptr)


transpose : List String -> List String
transpose lines =
    transposeHelper (paddingSpaces lines [] (maxLength lines 0)) [] 0
