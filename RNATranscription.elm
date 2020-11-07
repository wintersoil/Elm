module RNATranscription exposing (toRNA)
import Html
import Dict exposing (Dict)
import List



hm: Dict String String
hm = Dict.fromList
        [("G", "C"),
        ("C", "G"),
        ("T", "A"),
        ("A", "U")
        ]

complement: String -> String
complement a = Maybe.withDefault "Error"
      (Dict.get a hm)

toRNA : String -> Result String String
toRNA dna =
  if(List.member "Error" (List.map complement (String.split "" dna))) then
    Err "Error Invalid input"
  else
    Ok (String.join "" (List.map complement (String.split "" dna)))

main = Html.text (Result.withDefault "Error Invalid input" (toRNA "ACGTGGTCTTAA"))
