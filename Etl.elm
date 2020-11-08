module Etl exposing (transform)

import Dict exposing (Dict)

addInDict: Int -> Dict String Int -> String -> Dict String Int
addInDict d dt s=
    Dict.insert (String.toLower s) d dt

writeScore: List String -> Int -> Dict String Int -> Int -> Dict Int (List String) -> Dict String Int
writeScore listOfString d dt ptr input=
    if(ptr <= (List.length listOfString) - 1) then
        addInDict d (writeScore (Maybe.withDefault [] (Dict.get d input)) d dt (ptr+1) input) ( Maybe.withDefault "A" (List.head (List.drop ptr listOfString)))
    else
        dt


figureOutListString: Dict Int (List String) -> Dict String Int -> Int -> Dict String Int
figureOutListString input dt d=
    writeScore (Maybe.withDefault [] (Dict.get d input)) d dt 0 input

recursiveFunction: List Int -> Int -> Dict Int (List String) -> Dict String Int
recursiveFunction  listInts ptr input=
    if(ptr <= (List.length listInts) - 1) then
        figureOutListString input (recursiveFunction (Dict.keys input) (ptr+1) input) (Maybe.withDefault 0 (List.head (List.drop ptr listInts)))
    else
        figureOutListString input Dict.empty (Maybe.withDefault 0 (List.head (List.drop ptr listInts)))

transform : Dict Int (List String) -> Dict String Int
transform input =
    recursiveFunction (Dict.keys input) 0 input
