module SpaceAge exposing (Planet(..), ageOn)
import Dict exposing(Dict)
import Html



type Planet
    = Mercury
    | Venus
    | Earth
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune


timeRel: Dict String Float
timeRel = 
  Dict.fromList
  [
    ("Mercury", (0.2408467 * 31557600)),
    ("Venus", (0.61519726 * 31557600)),
    ("Earth", (1.0 * 31557600)),
    ("Mars", (1.8808158 * 31557600)),
    ("Jupiter", (11.862615 * 31557600)),
    ("Saturn", (29.447498 * 31557600)),
    ("Uranus", (84.016846 * 31557600)),
    ("Neptune", (164.79132 * 31557600))
  ]



pl : Planet -> String
pl planet =
  case planet of
    Mercury -> "Mercury"
    Venus -> "Venus"
    Earth -> "Earth"
    Mars -> "Mars"
    Jupiter -> "Jupiter"
    Saturn -> "Saturn"
    Uranus -> "Uranus"
    Neptune -> "Neptune"

ageOn : Planet -> Float -> Float
ageOn planet seconds = 
  (seconds / Maybe.withDefault 1.0 (Dict.get (pl planet) timeRel))
  
  
main = Html.text (String.fromFloat (ageOn Earth 1000000000))
  
