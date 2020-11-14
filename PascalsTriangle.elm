module Triangle exposing (rows)

reducer1 : List Int -> Int
reducer1 l1 =
    if (List.length l1 == 0) then
        -14
    else if (List.length l1 > 1) then
        (Maybe.withDefault -3 (List.head l1) + Maybe.withDefault -3 (List.head (List.drop 1 l1)))
    else
        -7


fillAList : Int -> List Int -> List Int -> List Int -> Int -> Int -> Int -> List Int
fillAList n listInitial list result ptr starting startingPos=
    if (startingPos <= 2 && ptr <= starting && List.length list <= ptr) then
        fillAList n (listInitial) (List.append [1] list) (List.append [1] result) (ptr+1) (starting) (startingPos + 1)
    else if ((ptr == 1 || ptr == starting) && List.length list <= ptr) then
        fillAList n (listInitial) (List.append [1] list) (List.append [1] result) (ptr+1) (starting) (startingPos + 1)
    else if (ptr > 1 && ptr < starting && List.length list <= ptr) then
        fillAList n (List.drop 1 listInitial) (List.append [(reducer1 listInitial)] list) (List.append [(reducer1 listInitial)] result) (ptr+1) (starting) (startingPos + 1)
    else
        result

helperRows : Int -> List Int -> List Int -> List (List Int) -> Int -> List (List Int)
helperRows n listInitial list result startingPos=
    if (startingPos <= n) then
        helperRows n (fillAList n listInitial list [] 1 startingPos startingPos) [] (List.append result [(fillAList n listInitial list [] 1 startingPos startingPos)]) (startingPos + 1)
    else
        result

rows : Int -> List (List Int)
rows n =
    helperRows n [] [] [] 1
