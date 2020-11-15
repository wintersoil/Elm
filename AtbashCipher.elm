module AtbashCipher exposing (decode, encode)


encodeHelper : String -> String -> String -> String -> Int -> String
encodeHelper plain plainal encodedal result ptr=
    if (modBy 6 ptr == 0 && String.length plain > 0) then
        encodeHelper (plain) plainal encodedal (result ++ " ") (ptr + 1)
    else if (String.length plain > 0 && Char.isAlpha (Maybe.withDefault '-' (List.head (String.toList(plain))))) then
        encodeHelper (String.slice 1 (String.length plain) plain) plainal encodedal (result ++ (String.slice (Maybe.withDefault 0 (List.head (String.indexes (String.slice 0 1 plain) plainal))) ((Maybe.withDefault 0 (List.head (String.indexes (String.slice 0 1 plain) plainal))) + 1) encodedal)) (ptr + 1)
    else if (String.length plain > 0 && Char.isAlphaNum (Maybe.withDefault '-' (List.head (String.toList(plain))))) then
        encodeHelper (String.slice 1 (String.length plain) plain) plainal encodedal (result ++ (String.slice 0 1 plain)) (ptr + 1)
    else if (String.length plain > 0 && (Char.isAlphaNum (Maybe.withDefault '-' (List.head (String.toList(plain))))) == False) then
        encodeHelper (String.slice 1 (String.length plain) plain) plainal encodedal (result) ptr
    else
        result

decodeHelper : String -> String -> String -> String -> Int -> String
decodeHelper plain plainal encodedal result ptr=
    if (modBy 6 ptr == 0 && String.length plain > 0) then
        decodeHelper (plain) plainal encodedal (result) (ptr + 1)
    else if (String.length plain > 0 && Char.isAlpha (Maybe.withDefault '-' (List.head (String.toList(plain))))) then
        decodeHelper (String.slice 1 (String.length plain) plain) plainal encodedal (result ++ (String.slice (Maybe.withDefault 0 (List.head (String.indexes (String.slice 0 1 plain) encodedal))) ((Maybe.withDefault 0 (List.head (String.indexes (String.slice 0 1 plain) encodedal))) + 1) plainal)) (ptr + 1)
    else if (String.length plain > 0 && Char.isAlphaNum (Maybe.withDefault '-' (List.head (String.toList(plain))))) then
        decodeHelper (String.slice 1 (String.length plain) plain) plainal encodedal (result ++ (String.slice 0 1 plain)) (ptr + 1)
    else if (String.length plain > 0 && (Char.isAlphaNum (Maybe.withDefault '-' (List.head (String.toList(plain))))) == False) then
        decodeHelper (String.slice 1 (String.length plain) plain) plainal encodedal (result) ptr
    else
        result


encode : String -> String
encode plain =
    String.trim (encodeHelper (String.toLower plain) ("abcdefghijklmnopqrstuvwxyz") ("zyxwvutsrqponmlkjihgfedcba") "" 1)

decode : String -> String
decode cipher =
    String.trim (decodeHelper (String.toLower cipher) ("abcdefghijklmnopqrstuvwxyz") ("zyxwvutsrqponmlkjihgfedcba") "" 1)
    
    
