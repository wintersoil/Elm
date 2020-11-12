module Acronym exposing (abbreviate)

shredAndTakeFirst : String -> String -> Bool -> Bool -> String
shredAndTakeFirst a b spaceOccurred firstAttempt=
    if (String.length a > 0 && ((String.slice 0 1 a) == " " || (String.slice 0 1 a) == "-" )) then
        shredAndTakeFirst (String.slice 1 (String.length a) a) b True False
    else if (String.length a > 0 && ((String.slice 0 1 a) == " ") == False && (spaceOccurred == True || firstAttempt)) then
        shredAndTakeFirst (String.slice 1 (String.length a) a) (String.append b (String.toUpper(String.slice 0 1 a))) False False
    else if (String.length a > 0 && ((String.slice 0 1 a) == " ") == False && spaceOccurred == False) then
        shredAndTakeFirst (String.slice 1 (String.length a) a) b False False
    else
        b


abbreviate : String -> String
abbreviate phrase =
    shredAndTakeFirst phrase "" False True
