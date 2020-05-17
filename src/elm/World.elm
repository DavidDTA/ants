module World exposing (Entity, EntityCoordinates, Glyph(..), World, WorldCoordinates, allEntities, init, playerFrame)

{-| The World of Ants
-}

import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Length exposing (Length)
import Point2d exposing (Point2d)


type Glyph
    = Anthill
    | Ant
    | PlayerAnt


type World
    = World
        { player : PlayerLocation
        , anthill : Point2d Length.Meters WorldCoordinates
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
