module Isogram exposing (isIsogram)
import Set exposing (Set)


fillSet : Char -> Set Char -> String -> Int -> Int -> Bool
fillSet ch set1 sentence lengthy countNonChars=
    if (Set.member (Char.toLower ch) set1 && Char.isAlpha ch) then
        False
    else if ((String.length sentence) == 1 && (Set.size set1 < lengthy - 1 - countNonChars)) then
        False
    else if ( (Set.member (Char.toLower ch) set1) == False && Char.isAlpha ch && (Set.size set1 == lengthy - 1 - countNonChars)) then
        True
    else if ( Char.isAlpha ch == False ) then
        fillSet (Maybe.withDefault '_' (List.head( List.drop 1 (String.toList sentence)))) set1 (String.slice 1 (String.length sentence) sentence) lengthy (countNonChars + 1)
    else
        fillSet (Maybe.withDefault '_' (List.head( List.drop 1 (String.toList sentence)))) (Set.insert (Char.toLower ch) set1) (String.slice 1 (String.length sentence) sentence) lengthy (countNonChars)

isIsogram : String -> Bool
isIsogram sentence =
    if (String.length sentence == 0) then
        True
    else
        fillSet (Maybe.withDefault '_' (List.head (String.toList sentence))) Set.empty sentence (String.length sentence) 0

