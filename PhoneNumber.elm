module PhoneNumber exposing (getNumber)

getCleaner : String -> String -> Int -> Bool -> Bool -> Maybe String
getCleaner phoneNumber gathererString iterationCount isBegOne firstNum=
    if (((Maybe.withDefault '_' (List.head (String.toList (phoneNumber)))) == '0' || (Maybe.withDefault '_' (List.head (String.toList (phoneNumber)))) == '1') && String.length (String.trim gathererString) == 3) then
        Nothing
    else if (String.length phoneNumber == 0 && String.length (String.trim gathererString) == 10 && ((Maybe.withDefault '_' (List.head (String.toList (gathererString)))) == '1') == False && ((Maybe.withDefault '_' (List.head (String.toList (gathererString)))) == '0') == False) then
        Just gathererString
    else if (String.length phoneNumber == 0 && String.length (String.trim gathererString) == 10 && (((Maybe.withDefault '_' (List.head (String.toList (gathererString)))) == '1') == True || ((Maybe.withDefault '_' (List.head (String.toList (gathererString)))) == '0') == True)) then
        Nothing
    else if (String.length phoneNumber == 0 && String.length (String.trim gathererString) >= 11) then
        Nothing
    else if (String.length phoneNumber == 0 && String.length (String.trim gathererString) < 10) then
        Nothing
    else if (Char.isDigit (Maybe.withDefault '_' (List.head (String.toList (phoneNumber)))) && firstNum == False && ((Maybe.withDefault '_' (List.head (String.toList (phoneNumber))) == '1') == False  && (Maybe.withDefault '_' (List.head (String.toList (phoneNumber))) == '0') == False )) then
        getCleaner (String.slice 1 (String.length phoneNumber) phoneNumber) (String.append (String.slice 0 1 (phoneNumber)) (gathererString)) (iterationCount + 1) False True
    else if (Char.isDigit (Maybe.withDefault '_' (List.head (String.toList (phoneNumber)))) && firstNum == False && ((Maybe.withDefault '_' (List.head (String.toList (phoneNumber))) == '1') || (Maybe.withDefault '_' (List.head (String.toList (phoneNumber))) == '0'))) then
        getCleaner (String.slice 1 (String.length phoneNumber) phoneNumber) ((gathererString)) (iterationCount + 1) True True
    else if (Char.isDigit (Maybe.withDefault '_' (List.head (String.toList (phoneNumber)))) == False) then
        getCleaner (String.slice 1 (String.length phoneNumber) phoneNumber) (gathererString) (iterationCount + 1) isBegOne firstNum
    else if (Char.isDigit (Maybe.withDefault '_' (List.head (String.toList (phoneNumber)))) && firstNum == True) then
        getCleaner (String.slice 1 (String.length phoneNumber) phoneNumber) (String.append (gathererString) (String.slice 0 1 (phoneNumber))) (iterationCount + 1) isBegOne True
    else if ((gathererString == "") == False) then
        Just gathererString
    else
        Nothing

getNumber : String -> Maybe String
getNumber phoneNumber =
    getCleaner phoneNumber "" 0 False False
