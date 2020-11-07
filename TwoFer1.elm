module TwoFer exposing (twoFer)
import Html

twoFer: Maybe String -> String
twoFer name =
  "One for " ++ Maybe.withDefault "you" name ++ ", one for me."

name1: Maybe String
name1 = Just "Ali"
main = Html.text (twoFer name1)
