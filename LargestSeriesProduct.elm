module LargestSeriesProduct exposing (largestProduct)
import Html

multiplyAllNumbers : String -> Int -> Int
multiplyAllNumbers series product =
    if (String.length series > 0) then
        multiplyAllNumbers (String.slice 1 (String.length series) series) (Maybe.withDefault 1 (String.toInt (String.slice 0 1 (series))) * product)
    else
        product

choppingStrings : Int -> String -> Int -> Int -> List Int -> Maybe Int
choppingStrings length series maxProduct product listy=
    if (String.length series == length && List.foldl (+) 0 ((product * (multiplyAllNumbers (String.slice 0 length series) product)) :: listy) == 0) then
        Just 0
    else if (String.length series >= length && maxProduct < (product * (multiplyAllNumbers (String.slice 0 length series) product))) then
        choppingStrings length (String.slice 1 (String.length(series)) series) (product * (multiplyAllNumbers (String.slice 0 length series) product)) 1 ((product * (multiplyAllNumbers (String.slice 0 length series) product)) :: listy)
    else if (String.length series >= length && maxProduct > (product * (multiplyAllNumbers (String.slice 0 length series) product))) then
        choppingStrings length (String.slice 1 (String.length(series)) series) maxProduct 1 ((product * (multiplyAllNumbers (String.slice 0 length series) product)) :: listy)
    else if (String.length series >= length && maxProduct == (product * (multiplyAllNumbers (String.slice 0 length series) product))) then
        choppingStrings length (String.slice 1 (String.length(series)) series) maxProduct 1 ((product * (multiplyAllNumbers (String.slice 0 length series) product)) :: listy)
    else
        Just maxProduct


largestProduct : Int -> String -> Maybe Int
largestProduct length series =
    if(List.length (List.filter Char.isAlpha (String.toList series)) > 0) then
        Nothing
    else if (length == 0 && String.length (series) > 0) then
        Just 1
    else if (length == 0 && String.length (series) == 0) then
        Just 1
    else if (String.length series < length) then
        Nothing
    else if (length <= 0) then
        Nothing
    else if (Maybe.withDefault -2 (String.toInt series) == 0) then
        Just 0
    else
        ( choppingStrings length series 1 1 [])

