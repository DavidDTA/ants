module World exposing (Entity, EntityCoordinates, Glyph(..), World, WorldCoordinates, allEntities, doFrame, init, playerFrame)

{-| The World of Ants
-}

import Angle
import Duration
import Frame2d exposing (Frame2d)
import Keyboard
import Keyboard.KeyCode
import Length exposing (Length)
import Point2d exposing (Point2d)
import Quantity
import Time


type Glyph
    = Anthill
    | Ant
    | PlayerAnt


type World
    = World
        { player : PlayerLocation
        , anthill : Point2d Length.Meters WorldCoordinates
        , updated : Maybe Time.Posix
        }


type PlayerLocation
    = AtHome
    | Exploring
        { frame : Frame2d Length.Meters WorldCoordinates { defines : EntityCoordinates }
        }


type alias Entity =
    { glyph : Glyph
    , frame : Frame2d Length.Meters WorldCoordinates { defines : EntityCoordinates }
    }


init =
    World
        { player =
            Exploring
                { frame = Frame2d.atOrigin
                }
        , anthill = Point2d.origin
        , updated = Nothing
        }


{-| The coordinate system for the 2-d world of ants. Positive x is East and positive y is North.
-}
type WorldCoordinates
    = WorldCoordinates


type EntityCoordinates
    = EntityCoordinates


playerFrame : World -> Maybe (Frame2d.Frame2d Length.Meters WorldCoordinates { defines : EntityCoordinates })
playerFrame (World { player }) =
    case player of
        AtHome ->
            Nothing

        Exploring { frame } ->
            Just frame


allEntities : World -> List Entity
allEntities (World { anthill, player }) =
    [ Just
        { glyph = Anthill
        , frame = Frame2d.atPoint anthill
        }
    , case player of
        AtHome ->
            Nothing

        Exploring { frame } ->
            Just { glyph = PlayerAnt, frame = frame }
    ]
        |> List.filterMap identity


doFrame : Time.Posix -> Keyboard.State -> World -> World
doFrame timestamp keyboard (World world) =
    case world.updated of
        Nothing ->
            World { world | updated = Just timestamp }

        Just previousTimestamp ->
            let
                diff =
                    Duration.from previousTimestamp timestamp
            in
            World
                { world
                    | updated = Just timestamp
                    , player = updatePlayer diff keyboard world
                }


playerTurnRate =
    Angle.degrees 60 |> Quantity.per Duration.second


playerSpeed =
    Length.centimeters 1 |> Quantity.per Duration.second


updatePlayer duration keyboard world =
    case world.player of
        AtHome ->
            world.player

        Exploring { frame } ->
            let
                leftPressed =
                    Keyboard.isPressed Keyboard.KeyCode.arrowLeft keyboard

                rightPressed =
                    Keyboard.isPressed Keyboard.KeyCode.arrowRight keyboard

                forwardPressed =
                    Keyboard.isPressed Keyboard.KeyCode.arrowUp keyboard

                angleScale =
                    case ( leftPressed, rightPressed ) of
                        ( True, False ) ->
                            1

                        ( False, True ) ->
                            -1

                        ( True, True ) ->
                            0

                        ( False, False ) ->
                            0

                angleChange =
                    playerTurnRate
                        |> Quantity.multiplyBy angleScale
                        |> Quantity.for duration

                speedScale =
                    if forwardPressed then
                        1

                    else if leftPressed || rightPressed then
                        0.5

                    else
                        0

                distanceTraveled =
                    playerSpeed
                        |> Quantity.multiplyBy speedScale
                        |> Quantity.for duration
            in
            Exploring
                { frame =
                    frame
                        |> Frame2d.rotateBy angleChange
                        |> Frame2d.translateAlongOwn Frame2d.yAxis distanceTraveled
                }
