module PythagoreanTriplet exposing (triplets)

type alias Triplet =
    ( Int, Int, Int )

initiateLoopOnce : Int -> List Int -> List Int -> List Triplet -> List Triplet
initiateLoopOnce n l1 l2 result=
    if (List.length l2 > 0 && (n^2 - (2 * n * Maybe.withDefault 0 (List.head l1)) - (2 * n * Maybe.withDefault 0 (List.head l2)) + (2 * Maybe.withDefault 0 (List.head l1) * Maybe.withDefault 0 (List.head l2))) == 0 ) then
        initiateLoopOnce n (List.drop 1 l1) (List.range (Maybe.withDefault 0 (List.head l1) + 1) (Maybe.withDefault 0 (List.head l2) - 1)) (List.append result [((Maybe.withDefault 0 (List.head l1)),(Maybe.withDefault 0 (List.head l2)),n - (Maybe.withDefault 0 (List.head l1)) - (Maybe.withDefault 0 (List.head l2)) )])
    else if (List.length l2 > 0 && ((Maybe.withDefault 0 (List.head l1)) + (Maybe.withDefault 0 (List.head l2)) > n)) then
        initiateLoopOnce n (List.drop 1 l1) (List.range (Maybe.withDefault 0 (List.head l1) + 1) (Maybe.withDefault 0 (List.head l2) - 1)) (result)
    else if (List.length l2 > 0 && ((n^2 - (2 * n * Maybe.withDefault 0 (List.head l1)) - (2 * n * Maybe.withDefault 0 (List.head l2)) + (2 * Maybe.withDefault 0 (List.head l1) * Maybe.withDefault 0 (List.head l2))) == 0) == False) then
        initiateLoopOnce n (l1) (List.drop 1 l2) (result)
    else if ((List.length l2 > 0) == False && (List.length l1 > 0) == False) then
        result
    else if ((List.length l2 > 0) == False) then
        initiateLoopOnce n (List.drop 1 l1) (List.drop 1 (List.drop 1 l1)) (result)
    else
        result


triplets : Int -> List Triplet
triplets n =
    initiateLoopOnce n (List.range 1 ((n)//2)) (List.range 2 ((n)//2)) []
