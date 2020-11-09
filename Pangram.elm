module Pangram exposing (isPangram)
import Dict exposing (Dict)
import Set

placeInDict: Char -> Dict Char Int -> Dict Char Int
placeInDict ch dt=
    Dict.insert ch (Maybe.withDefault 1 (Dict.get ch dt)) dt

verifyAllAlphabets: Dict Char Int -> Bool
verifyAllAlphabets dt=
    List.length (Set.toList ( Set.fromList (List.filter Char.isAlpha (List.map Char.toLower (Dict.keys dt))))) == 26

fillDict : List Char -> Dict Char Int -> Bool
fillDict chs dt=
    if(List.length chs == 0) then
        verifyAllAlphabets (dt)
    else
        fillDict (List.drop 1 chs) ( placeInDict (Maybe.withDefault '.' (List.head chs)) dt)

isPangram : String -> Bool
isPangram sentence =
    fillDict ( String.toList sentence ) Dict.empty

