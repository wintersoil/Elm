module Raindrops exposing (raindrops)

addPlong : String -> String
addPlong s1 =
    String.append s1 "Plong"

checkSeven : Int -> String -> String
checkSeven number result=
    if(modBy 7 number == 0) then
        addPlong result
    else
        result


addPlang : String -> String
addPlang s1 =
    String.append s1 "Plang"

checkFive : Int -> String -> String
checkFive number result=
    if(modBy 5 number == 0) then
        addPlang result
    else
        result


addPling : String -> String
addPling s1 =
    String.append s1 "Pling"

checkThree : Int -> String -> String
checkThree number result=
    if(modBy 3 number == 0) then
        addPling result
    else
        result

addNumber : String -> Int -> String
addNumber s1 i1 =
    String.append (String.fromInt i1) s1

raindrops : Int -> String
raindrops number =
    if (String.length (checkSeven number (checkFive number (checkThree number ""))) > 0 ) then
        checkSeven number (checkFive number (checkThree number ""))
    else
        addNumber "" number
