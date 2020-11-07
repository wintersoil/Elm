module DifferenceOfSquares exposing (difference, squareOfSum, sumOfSquares)
import Html

squareOfSum : Int -> Int
squareOfSum n = List.sum (List.range 1 n) * List.sum (List.range 1 n)


sumOfSquares : Int -> Int
sumOfSquares n = List.sum (List.map accumulateSquare (List.range 1 n))

accumulateSquare : Int -> Int
accumulateSquare b =
    ( b * b )

difference : Int -> Int
difference n =
    squareOfSum(n) - sumOfSquares(n)

main =
  Html.text (String.fromInt (difference 5))
