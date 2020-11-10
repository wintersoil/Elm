module ArmstrongNumbers exposing (isArmstrongNumber)

numberOfDigits: Int -> Int
numberOfDigits nb =
    String.length ( String.fromInt nb )

divideAndConquer: Int -> Int -> Int -> Int
divideAndConquer nb lengthy sum=
    if ( nb > 0 ) then
        divideAndConquer (nb // 10) lengthy (sum + (modBy 10 nb) ^ lengthy )
    else
        sum


isArmstrongNumber : Int -> Bool
isArmstrongNumber nb =
    if ( nb == divideAndConquer nb ( numberOfDigits nb ) 0 ) then
        True
    else
        False
        
