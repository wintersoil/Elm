module Allergies exposing (Allergy(..), isAllergicTo, toList)
import Dict exposing (Dict)

type Allergy
    = Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats

isAllergicTo : Allergy -> Int -> Bool
isAllergicTo allergy score =
    if (List.member allergy (computeList score [])) then
        True
    else
        False


computeList : Int -> List Allergy -> List Allergy
computeList score lA =
    if (score >= 256) then
        computeList (score - 256) (lA)
    else if (score >= 128) then
        computeList (score - 128) (Cats :: lA)
    else if (score >= 64) then
        computeList (score - 64) (Pollen :: lA)
    else if (score >= 32) then
        computeList (score - 32) (Chocolate :: lA)
    else if (score >= 16) then
        computeList (score - 16) (Tomatoes :: lA)
    else if (score >= 8) then
        computeList (score - 8) (Strawberries :: lA)
    else if (score >= 4) then
        computeList (score - 4) (Shellfish :: lA)
    else if (score >= 2) then
        computeList (score - 2) (Peanuts :: lA)
    else if (score >= 1) then
        computeList (score - 1) (Eggs :: lA)
    else
        lA

toList : Int -> List Allergy
toList score =
    computeList score []
