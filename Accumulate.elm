module Accumulate exposing (accumulate)
import Html

accumulate : (a -> b) -> List a -> List b
accumulate func input =
    List.map func input

main = Html.text (String.join ", " (accumulate String.toUpper [ "hello", "world" ]))
