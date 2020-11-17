module MatchingBrackets exposing (isPaired)

combineOrCloseAndThrow : String -> String -> Int -> Bool
combineOrCloseAndThrow input result initialLength=
    if (initialLength == 0 && String.length input > 0 && ((String.slice 0 1 input) == "}" || (String.slice 0 1 input) == ")" || (String.slice 0 1 input) == "]")) then
        False
    else if (String.length input > 0 && ((String.slice 0 1 input) == "{" || (String.slice 0 1 input) == "[" || (String.slice 0 1 input) == "(")) then
        combineOrCloseAndThrow (String.slice 1 (String.length input) input) (String.append result (String.slice 0 1 input)) (initialLength + 1)
    else if (String.length input > 0 && (( (String.slice ((String.length result) - 1) ((String.length result))  result) == "{" && (String.slice 0 1 input) == "}") || ( (String.slice ((String.length result) - 1) ((String.length result))  result) == "[" && (String.slice 0 1 input) == "]") || ( (String.slice ((String.length result) - 1) ((String.length result))  result) == "(" && (String.slice 0 1 input) == ")")) ) then
        combineOrCloseAndThrow (String.slice 1 (String.length input) input) (String.slice 0 ((String.length result) - 1) result) (initialLength + 1)
    else if (String.length input > 0 && (((String.slice 0 1 input) == "[") == False && ((String.slice 0 1 input) == "{") == False && ((String.slice 0 1 input) == "(") == False && ((String.slice 0 1 input) == "]") == False && ((String.slice 0 1 input) == "}") == False && ((String.slice 0 1 input) == ")") == False )) then
        combineOrCloseAndThrow (String.slice 1 (String.length input) input) result (initialLength + 1)
    else if (String.length input > 0 && (( (String.slice ((String.length result) - 1) ((String.length result))  result) == "{" && ((String.slice 0 1 input) == "}") == False) || ( (String.slice ((String.length result) - 1) ((String.length result))  result) == "[" && ((String.slice 0 1 input) == "]") == False) || ( (String.slice ((String.length result) - 1) ((String.length result))  result) == "(" && ((String.slice 0 1 input) == ")") == False)) ) then
        False
    else if (String.length result == 0 && String.length input == 0) then
        True
    else if ((String.length result == 0) == False && String.length input == 0) then
        False
    else
        True

isPaired : String -> Bool
isPaired input =
    combineOrCloseAndThrow input "" 0
