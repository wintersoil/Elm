module BinarySearch exposing (find)

import Array exposing (Array)

findChoppedOffCarriedForward : Int -> Array Int -> Int -> Maybe Int
findChoppedOffCarriedForward target xs choppedLeft=
        if (Array.length xs == 1 && (Maybe.withDefault -1 (Array.get 0 xs) == target)) then
            Just (0 + choppedLeft)
        else if (Array.length xs == 0) then
            Nothing
        else if (target > Maybe.withDefault -1 (Array.get ((Array.length xs) // 2) xs) &&  (target == Maybe.withDefault -1(Array.get ((Array.length xs) // 2) xs)) == False ) then
            findChoppedOffCarriedForward target (Array.slice ( (Array.length xs) // 2 + 1 ) (Array.length xs) xs)  ((( (Array.length xs) // 2 + 1 ) - 0) + choppedLeft)
        else if (target < Maybe.withDefault -1 (Array.get ((Array.length xs) // 2) xs) &&  (target == Maybe.withDefault -1(Array.get ((Array.length xs) // 2) xs)) == False ) then
            findChoppedOffCarriedForward target ( Array.slice 0 ((Array.length xs) // 2) xs ) choppedLeft
        else
            Just ((Array.length xs) // 2 + choppedLeft)


find : Int -> Array Int -> Maybe Int
find target xs =
    findChoppedOffCarriedForward target xs 0
