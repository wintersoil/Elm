module TwelveDays exposing (recite)
import Dict exposing (Dict)

d12: Dict Int String
d12 =
    Dict.fromList
    [
        (1, "a Partridge in a Pear Tree"),
        (2, "two Turtle Doves"),
        (3, "three French Hens"),
        (4, "four Calling Birds"),
        (5, "five Gold Rings"),
        (6, "six Geese-a-Laying"),
        (7, "seven Swans-a-Swimming"),
        (8, "eight Maids-a-Milking"),
        (9, "nine Ladies Dancing"),
        (10, "ten Lords-a-Leaping"),
        (11, "eleven Pipers Piping"),
        (12, "twelve Drummers Drumming")
    ]

ddays: Dict Int String
ddays =
    Dict.fromList
    [
        (1, "first"),
        (2, "second"),
        (3, "third"),
        (4, "fourth"),
        (5, "fifth"),
        (6, "sixth"),
        (7, "seventh"),
        (8, "eighth"),
        (9, "ninth"),
        (10, "tenth"),
        (11, "eleventh"),
        (12, "twelfth")
    ]

extractItem : Int -> List String -> List String
extractItem ptr list =
    ( List.append [(Maybe.withDefault "____" (Dict.get ptr d12))] list )

extractItem2 : Int -> String
extractItem2 ptr =
    (Maybe.withDefault "____" (Dict.get ptr d12))

gatherStringsDeeper: Int -> Int -> String -> String
gatherStringsDeeper ptr begPtr result =
    if( ptr == begPtr) then
        gatherStringsDeeper (ptr - 1) (begPtr) (String.append (result) (extractItem2 ptr) )
    else if( ptr == 1) then
        gatherStringsDeeper (ptr - 1) (begPtr) (String.append (result ++ ", and ") (extractItem2 ptr) )
    else if( ptr > 0 ) then
        gatherStringsDeeper (ptr - 1) (begPtr) (String.append (result ++ ", " ) (extractItem2 ptr) )
    else
        result

gatherStrings : List Int -> List String -> List String
gatherStrings range list=
    if (List.length range > 0) then
        gatherStrings (List.drop 1 range) (List.append [gatherStringsDeeper (Maybe.withDefault 3 (List.head range)) (Maybe.withDefault 3 (List.head range)) ""] list )
    else
        list

extractItemDays : Int -> String
extractItemDays ptr =
    (Maybe.withDefault "____" (Dict.get ptr ddays))

gatherStringsDays : List Int -> List String -> List String
gatherStringsDays range list=
    if (List.length range > 0) then
        gatherStringsDays (List.drop 1 range) (List.append [String.append (String.append "On the " (extractItemDays (Maybe.withDefault 3 (List.head range)))) " day of Christmas my true love gave to me: "] list)
    else
        list

combineTwoListStrings : List String -> List String -> List String -> List String
combineTwoListStrings list1 list2 result=
    if ( List.length list1 > 0 ) then
        combineTwoListStrings (List.drop 1 list1) (List.drop 1 list2) (List.append [String.append (Maybe.withDefault "___" (List.head list1)) (Maybe.withDefault "___" (List.head list2) ++ ".")] result )
    else
        result

recite : Int -> Int -> List String
recite start stop =
    combineTwoListStrings (gatherStringsDays ( List.range start stop ) []) (gatherStrings ( List.range start stop ) []) []
