module AllYourBase exposing (rebase)
import Html

findHighestPowerDivisor : Int -> Int -> Int -> Int
findHighestPowerDivisor whole outBase higher=
    if (whole >= (outBase ^ higher)) then
        findHighestPowerDivisor whole outBase (higher + 1)
    else
        (higher - 1)

separatorCommon : Int -> Int -> Int -> Int -> Int
separatorCommon whole outBase inBase result =
    if ( whole > 0 ) then
        separatorCommon (whole - (outBase ^ (findHighestPowerDivisor (whole) outBase 0))) outBase inBase (result + (outBase ^ (findHighestPowerDivisor (whole) outBase 0)))
    else
        result


accumulatorCommon : Int -> List Int -> Int -> Int -> Int
accumulatorCommon inBase digits sum power =
    if (List.length digits > 0) then
        accumulatorCommon inBase (List.drop 1 digits) (sum + ((Maybe.withDefault -1 (List.head digits)) * (inBase ^ power))) (power + 1)
    else
        sum

separateInts : Int -> List Int -> Int -> List Int
separateInts numbers l outBase =
    if (numbers > 0) then
        separateInts (numbers // outBase) (modBy outBase numbers :: l) outBase
    else
        l

rebase : Int -> List Int -> Int -> Maybe (List Int)
rebase inBase digits outBase =
    if (List.length digits == 0) then
        Nothing
    else if (List.length (List.filter (\x -> x < 0 || x >= inBase) digits) > 0) then
        Nothing
    else if (List.length (List.filter (\x -> x == 0) digits) == List.length (digits)) then
        Nothing
    else if (outBase <= 0 || inBase <= 0) then
        Nothing
    else
        Just (separateInts (separatorCommon (accumulatorCommon inBase (List.reverse digits) 0 0) outBase inBase 0) [] outBase)
        
        
