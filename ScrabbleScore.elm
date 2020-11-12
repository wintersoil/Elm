module ScrabbleScore exposing (scoreWord)

figureLetterOut : Char -> Int -> Int
figureLetterOut ch sum=
    if (Char.toLower ch == 'a' || Char.toLower ch == 'e' || Char.toLower ch == 'i' || Char.toLower ch == 'o' || Char.toLower ch == 'u' || Char.toLower ch == 'l' || Char.toLower ch == 'n' || Char.toLower ch == 'r' || Char.toLower ch == 's' || Char.toLower ch == 't') then
        sum + 1
    else if (Char.toLower ch == 'd' || Char.toLower ch == 'g') then
        sum + 2
    else if (Char.toLower ch == 'b' || Char.toLower ch == 'c' || Char.toLower ch == 'm' || Char.toLower ch == 'p') then
        sum + 3
    else if (Char.toLower ch == 'f' || Char.toLower ch == 'h' || Char.toLower ch == 'v' || Char.toLower ch == 'w' || Char.toLower ch == 'y') then
        sum + 4
    else if (Char.toLower ch == 'k') then
        sum + 5
    else if (Char.toLower ch == 'j' || Char.toLower ch == 'x') then
        sum + 8
    else if (Char.toLower ch == 'q' || Char.toLower ch == 'z') then
        sum + 10
    else
        sum


chopLetterBeg : String -> Int -> Int
chopLetterBeg y sum =
    if ((String.length y) > 0) then
        chopLetterBeg (String.slice 1 (String.length y) y) ((figureLetterOut (Maybe.withDefault '_' (List.head (String.toList y))) sum))
    else
        sum


scoreWord : String -> Int
scoreWord x =
    chopLetterBeg x 0

