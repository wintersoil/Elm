module WordCount exposing (wordCount)

import Dict exposing (Dict)

findNextWord : String -> String  -> String
findNextWord sentence word =
    if (String.length sentence > 0 && (Char.isAlphaNum (Maybe.withDefault '_' (List.head (String.toList sentence))) || (Maybe.withDefault '_' (List.head (String.toList sentence))) == '\'' )) then
        findNextWord (String.slice 1 (String.length sentence) sentence) (String.append word (String.slice 0 1 sentence))
    else
        word

populateDict : String -> String -> Dict String Int -> Dict String Int
populateDict sentence word dt =
    if ((String.slice 0 1 (findNextWord sentence "") == "\'") && String.slice ((String.length (findNextWord sentence "")) - 1) (String.length (findNextWord sentence "")) (findNextWord sentence "") == "\'") then
        populateDict (String.slice (Maybe.withDefault 0 (List.head (String.indexes (findNextWord sentence "") sentence)) + (String.length (findNextWord sentence ""))) (String.length sentence) sentence) word (Dict.insert (String.slice 1 ((String.length (findNextWord sentence "")) - 1) (String.toLower (findNextWord sentence ""))) (Maybe.withDefault 0 ((Dict.get (String.toLower (findNextWord sentence "")) dt))+ 1 ) dt)
    else if (String.length sentence > 0 && (String.length (findNextWord sentence "") > 0)) then
        populateDict (String.slice (Maybe.withDefault 0 (List.head (String.indexes (findNextWord sentence "") sentence)) + (String.length (findNextWord sentence ""))) (String.length sentence) sentence) word (Dict.insert (String.toLower (findNextWord sentence "")) (Maybe.withDefault 0 ((Dict.get (String.toLower (findNextWord sentence "")) dt))+ 1 ) dt)
    else if (String.length sentence > 0 && (String.length (findNextWord sentence "") == 0)) then
        populateDict (String.slice 1 (String.length sentence) sentence) word (dt)
    else
        dt


wordCount : String -> Dict String Int
wordCount sentence =
    populateDict sentence "" Dict.empty
