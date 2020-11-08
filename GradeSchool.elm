module GradeSchool exposing (addStudent, allStudents, empty, studentsInGrade)

import Dict exposing (Dict)


type alias Grade =
    Int


type alias Student =
    String


type alias School =
    Dict Grade (List Student)


empty : School
empty =
    Dict.empty


addStudent : Grade -> Student -> School -> School
addStudent grade student school =
    Dict.insert grade (student :: Maybe.withDefault [] (Dict.get grade school))  school


studentsInGrade : Grade -> School -> List Student
studentsInGrade grade school =
    List.sort (Maybe.withDefault [] (Dict.get grade school))

figureGrade : List Grade -> Int -> Grade
figureGrade ls i=
    Maybe.withDefault 0 (List.head (List.drop (i) ls))

allStudents : School -> List ( Grade, List Student )
allStudents school =
    List.indexedMap (\i x -> ((figureGrade (Dict.keys school) i), (studentsInGrade (figureGrade (Dict.keys school) i) school))) (Dict.toList school)
    
    
