module RunLengthEncoding exposing (decode, encode)

count : String -> Char -> Int -> Int
count string ch result=
    if (String.slice 0 1 string == (String.fromChar ch)) then
        count (String.slice 1 (String.length string) string) ch (result + 1)
    else
        result

convertToRepeatedString : Char -> Int -> String -> String
convertToRepeatedString ch repeat s1=
    if (repeat > 1) then
        convertToRepeatedString ch (repeat - 1) (String.append s1 (String.fromChar ch))
    else
        s1

encodeHelper : String -> String -> String
encodeHelper string result =
    if (String.length string > 0 && (count (String.slice 1 (String.length string) string) (Maybe.withDefault 'a' (List.head (String.toList string))) 0) >= 1) then
        encodeHelper (String.slice ((count (String.slice 1 (String.length string) string) (Maybe.withDefault 'a' (List.head (String.toList string))) 0) + 1) (String.length string) string) (result ++ (String.fromInt ((count (String.slice 1 (String.length string) string) (Maybe.withDefault 'a' (List.head (String.toList string))) 0) + 1) ) ++ (String.fromChar (Maybe.withDefault 'a' (List.head (String.toList string)))))
    else if (String.length string > 0 && (count (String.slice 1 (String.length string) string) (Maybe.withDefault 'a' (List.head (String.toList string))) 0) == 0) then
        encodeHelper (String.slice ((count (String.slice 1 (String.length string) string) (Maybe.withDefault 'a' (List.head (String.toList string))) 0) + 1) (String.length string) string) (result ++ (String.fromChar (Maybe.withDefault 'a' (List.head (String.toList string)))))
    else
        result

encode : String -> String
encode string=
    encodeHelper string ""

whileIsDigit : String -> String -> String
whileIsDigit s1 s2 =
    if (Char.isDigit (Maybe.withDefault 'c' (List.head (String.toList s1)))) then
        whileIsDigit (String.slice 1 (String.length s1) s1) (s2 ++ (String.slice 0 1 s1))
    else
        s2

decodeHelper : String -> String -> String
decodeHelper string result =
    if (String.length string > 0 && Char.isDigit (Maybe.withDefault 'e' (List.head (String.toList string))) ) then
        decodeHelper (String.slice (String.length(whileIsDigit string "")) (String.length string) string) (result ++ (convertToRepeatedString (Maybe.withDefault 'c' (List.head (List.drop (String.length(whileIsDigit string "")) (String.toList string)))) (Maybe.withDefault 0 (String.toInt (whileIsDigit string ""))) ""))
    else if (String.length string > 0 && Char.isDigit (Maybe.withDefault 'e' (List.head (String.toList string))) == False) then
        decodeHelper (String.slice 1 (String.length string) string) (result ++ (String.slice 0 1 string) )
    else
        result

decode : String -> String
decode string =
    decodeHelper string ""
