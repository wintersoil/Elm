module NucleotideCount exposing (nucleotideCounts)


type alias NucleotideCounts =
    { a : Int
    , t : Int
    , c : Int
    , g : Int
    }

fillDict : NucleotideCounts -> Char -> NucleotideCounts
fillDict nc ch=
    if(Char.toLower ch == 'a') then
        NucleotideCounts ((nc.a) + 1) nc.t nc.c nc.g
    else if(Char.toLower ch == 't') then
        NucleotideCounts nc.a ((nc.t) +1) nc.c nc.g
    else if(Char.toLower ch == 'c') then
        NucleotideCounts nc.a nc.t ((nc.c) + 1) nc.g
    else if(Char.toLower ch == 'g') then
        NucleotideCounts nc.a nc.t nc.c ((nc.g) + 1)
    else
        NucleotideCounts nc.a nc.t nc.c nc.g

recursiveInsert : NucleotideCounts -> String -> NucleotideCounts
recursiveInsert nc sequence=
    if(String.length sequence > 0) then
        recursiveInsert (fillDict nc (Maybe.withDefault 'z' (List.head (String.toList sequence)))) (String.fromList (List.drop 1 (String.toList sequence)))
    else
        nc

nucleotideCounts : String -> NucleotideCounts
nucleotideCounts sequence =
    recursiveInsert (NucleotideCounts 0 0 0 0) sequence
