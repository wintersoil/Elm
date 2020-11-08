module SumOfMultiples exposing (sumOfMultiples)
import Html
import Set exposing(Set)

verifyLimit : Int -> Int -> Int -> Bool
verifyLimit limit ptr d=
  if(d * ptr < limit) then
    True
  else
    False

recursingLimitChecker : List Int -> Int -> Int -> Int -> List Int
recursingLimitChecker appending limit ptr d=
  if(verifyLimit limit ptr d) then
    recursingLimitChecker (List.concat [[(d*ptr)], appending]) limit (ptr+1) d
  else
    appending

sumOfMultiples2 : List Int -> Int -> Int -> List Int -> List Int
sumOfMultiples2 divisors limit ptr appending =
   List.foldr (++) [] (List.map (recursingLimitChecker appending limit ptr ) divisors)

sumOfMultiples : List Int -> Int -> Int
sumOfMultiples divisors limit = List.sum (Set.toList (Set.fromList(sumOfMultiples2 divisors limit 1 [])))

main = Html.text(String.fromInt (sumOfMultiples [ 43, 47 ] 10000))
