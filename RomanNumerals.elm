module RomanNumerals exposing (toRoman)

recurseAppend : Int -> String -> String
recurseAppend number result =
    if (number >= 1000) then
        recurseAppend (number - 1000) (String.append result "M")
    else if (number >= 900) then
        recurseAppend (number - 900) (String.append result "CM")
    else if (number >= 500) then
        recurseAppend (number - 500) (String.append result "D")
    else if (number >= 400) then
        recurseAppend (number - 400) (String.append result "CD")
    else if (number >= 100) then
        recurseAppend (number - 100) (String.append result "C")
    else if (number >= 90) then
        recurseAppend (number - 90) (String.append result "XC")
    else if (number >= 50) then
        recurseAppend (number - 50) (String.append result "L")
    else if (number >= 40) then
        recurseAppend (number - 40) (String.append result "XL")
    else if (number >= 10) then
        recurseAppend (number - 10) (String.append result "X")
    else if (number >= 9) then
        recurseAppend (number - 9) (String.append result "IX")
    else if (number >= 5) then
        recurseAppend (number - 5) (String.append result "V")
    else if (number >= 4) then
        recurseAppend (number - 4) (String.append result "IV")
    else if (number >= 1) then
        recurseAppend (number - 1) (String.append result "I")
    else
        result

toRoman : Int -> String
toRoman number =
    recurseAppend number ""

