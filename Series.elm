module Series exposing (slices)

getSlice : Int -> String -> List Int -> List Int
getSlice size input result=
    if (String.length input > 0 && size > 0) then
        getSlice (size - 1) ( String.slice 1 ( String.length input ) input ) (List.append result [(Maybe.withDefault -3 (String.toInt (String.slice 0 1 input)))] )
    else
        result


slicesAccumulator : Int -> String -> List (List Int) -> List (List Int)
slicesAccumulator size input result =
    if ( String.length input >= size) then
        slicesAccumulator size (String.slice 1 (String.length input) input) (List.append result [(getSlice size input [])])
    else
        result

slices : Int -> String -> Result String (List (List Int))
slices size input =
    if ( String.length input == 0 ) then
            Err "series cannot be empty"
    else if ( size > String.length input ) then
        Err "slice length cannot be greater than series length"
    else if ( size == 0 ) then
        Err "slice length cannot be zero"
    else if ( size < 0 ) then
        Err "slice length cannot be negative"
    else
        Ok (slicesAccumulator size input [])
