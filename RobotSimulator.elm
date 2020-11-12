module RobotSimulator exposing
    ( Bearing(..)
    , Robot
    , advance
    , defaultRobot
    , simulate
    , turnLeft
    , turnRight
    )


type Bearing
    = North
    | East
    | South
    | West


type alias Robot =
    { bearing : Bearing
    , coordinates : { x : Int, y : Int }
    }


defaultRobot : Robot
defaultRobot =
    { bearing = North
    , coordinates = { x = 0, y = 0 }
    }


turnRight : Robot -> Robot
turnRight robot =
    if (robot.bearing == North) then
        { bearing = East, coordinates = { x = robot.coordinates.x, y = robot.coordinates.y} }
    else if (robot.bearing == East) then
        { bearing = South, coordinates = { x = robot.coordinates.x, y = robot.coordinates.y} }
    else if (robot.bearing == South) then
        { bearing = West, coordinates = { x = robot.coordinates.x, y = robot.coordinates.y} }
    else
        { bearing = North, coordinates = { x = robot.coordinates.x, y = robot.coordinates.y} }


turnLeft : Robot -> Robot
turnLeft robot =
    if (robot.bearing == North) then
        { bearing = West, coordinates = { x = robot.coordinates.x, y = robot.coordinates.y} }
    else if (robot.bearing == West) then
        { bearing = South, coordinates = { x = robot.coordinates.x, y = robot.coordinates.y} }
    else if (robot.bearing == South) then
        { bearing = East, coordinates = { x = robot.coordinates.x, y = robot.coordinates.y} }
    else
        { bearing = North, coordinates = { x = robot.coordinates.x, y = robot.coordinates.y} }


advance : Robot -> Robot
advance robot =
    if (robot.bearing == North) then
        { bearing = North, coordinates = { x = robot.coordinates.x, y = robot.coordinates.y + 1} }
    else if (robot.bearing == West) then
        { bearing = West, coordinates = { x = robot.coordinates.x - 1, y = robot.coordinates.y} }
    else if (robot.bearing == East) then
        { bearing = East, coordinates = { x = robot.coordinates.x + 1, y = robot.coordinates.y} }
    else
        { bearing = South, coordinates = { x = robot.coordinates.x, y = robot.coordinates.y - 1} }


simulate : String -> Robot -> Robot
simulate directions robot =
    if (String.slice 0 1 directions == "R") then
        simulate (String.slice 1 (String.length directions) directions) (turnRight robot)
    else if (String.slice 0 1 directions == "A") then
        simulate (String.slice 1 (String.length directions) directions) (advance robot)
    else if (String.slice 0 1 directions == "L") then
        simulate (String.slice 1 (String.length directions) directions) (turnLeft robot)
    else
        robot
