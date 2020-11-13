module Anagram exposing (detect)
import Dict exposing (Dict)

makeDict : String -> Dict Char Int -> Dict Char Int
makeDict word dt=
    if (String.length word > 0 && Dict.member (Maybe.withDefault '-' (List.head (String.toList word))) dt == False) then
        makeDict (String.slice 1 (String.length word) word) (Dict.insert (Maybe.withDefault '_' (List.head (String.toList word))) 1 dt)
    else if (String.length word > 0 && Dict.member (Maybe.withDefault '-' (List.head (String.toList word))) dt == True) then
        makeDict (String.slice 1 (String.length word) word) (Dict.insert (Maybe.withDefault '_' (List.head (String.toList word))) ((Maybe.withDefault 0 (Dict.get (Maybe.withDefault '_' (List.head (String.toList word))) dt) + 1)) dt)
    else
        dt

verifySameValuesForKeys: Char -> Dict Char Int -> Dict Char Int -> Bool
verifySameValuesForKeys ch dtw dtc =
    if (Dict.get ch dtw == Dict.get ch dtc) then
        True
    else
        False

forEachChar: String -> Dict Char Int -> Dict Char Int -> Bool -> Bool
forEachChar word dtw dtc hasEncounteredFalse=
    if (String.length word > 0 && verifySameValuesForKeys (Maybe.withDefault '_' (List.head (String.toList word))) dtw dtc) then
        forEachChar (String.slice 1 (String.length word ) word) dtw dtc hasEncounteredFalse
    else if (String.length word > 0 && (verifySameValuesForKeys (Maybe.withDefault '_' (List.head (String.toList word))) dtw dtc == False)) then
        forEachChar (String.slice 1 (String.length word ) word) dtw dtc False
    else
        hasEncounteredFalse

addToList : String -> List String -> List String -> List String
addToList word candidates result =
    if (( word == (String.toLower (Maybe.withDefault "___" (List.head candidates)))) == False && List.length candidates > 0 && Dict.size (Dict.diff (makeDict word Dict.empty) (makeDict (String.toLower (Maybe.withDefault "___" (List.head candidates))) Dict.empty)) == 0 && Dict.size (Dict.diff (makeDict (String.toLower (Maybe.withDefault "___" (List.head candidates)) ) Dict.empty) (makeDict word Dict.empty) ) == 0 && (forEachChar word (makeDict word Dict.empty) (makeDict (String.toLower (Maybe.withDefault "___" (List.head candidates))) Dict.empty) True) == True) then
        addToList word (List.drop 1 candidates) ((Maybe.withDefault "___" (List.head candidates)) :: result)
    else if (( word == (String.toLower (Maybe.withDefault "___" (List.head candidates)))) == True && List.length candidates > 0 && (forEachChar word (makeDict word Dict.empty) (makeDict (String.toLower (Maybe.withDefault "___" (List.head candidates))) Dict.empty) True) == True) then
        addToList word (List.drop 1 candidates) (result)
    else if (( word == (String.toLower (Maybe.withDefault "___" (List.head candidates)))) == False && List.length candidates > 0 && (forEachChar word (makeDict word Dict.empty) (makeDict (String.toLower (Maybe.withDefault "___" (List.head candidates))) Dict.empty) True) == False) then
        addToList word (List.drop 1 candidates) (result)
    else
        List.reverse result

detect : String -> List String -> List String
detect word candidates =
    addToList (String.toLower word) (candidates) []

