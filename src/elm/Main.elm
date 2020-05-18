module Main exposing (..)

import Browser
import Browser.Events
import Css
import Css.Extra
import Css.Global
import Direction2d
import Frame2d
import Framerate
import Html.Styled
import Html.Styled.Attributes
import Html.Styled.Events
import Json.Decode
import Keyboard
import Keyboard.KeyCode
import Length
import Pixels
import Point2d
import Quantity exposing (Quantity(..))
import Svg.Styled
import Svg.Styled.Attributes
import Time exposing (Posix)
import World


type Screen
    = Title
    | Gameplay
        { world : World.World
        }


type alias Model =
    { screen : Screen
    , framerate : Framerate.Framerate
    , keyboard : Keyboard.State
    }


type Msg
    = StartGame
    | AnimationFrame Posix
    | Keyboard Keyboard.Msg


type ViewportCoordinates
    = ViewportCoordinates


main =
    Browser.document
        { init = init
        , view = viewBody >> Browser.Document "Ants"
        , update = update
        , subscriptions = subscriptions
        }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init _ =
    ( { screen = Title, framerate = Framerate.init, keyboard = Keyboard.init }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            case model.screen of
                Title ->
                    ( { model | screen = Gameplay { world = World.init } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AnimationFrame frametime ->
            ( { model
                | framerate = Framerate.update frametime model.framerate
                , screen =
                    case model.screen of
                        Title ->
                            model.screen

                        Gameplay { world } ->
                            Gameplay { world = World.doFrame frametime model.keyboard world }
              }
            , Cmd.none
            )

        Keyboard keyboardMessage ->
            ( { model
                | keyboard = Keyboard.update keyboardMessage model.keyboard
              }
            , Cmd.none
            )


subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame AnimationFrame
        , Browser.Events.onKeyDown (Keyboard.event Keyboard)
        , Browser.Events.onKeyUp (Keyboard.event Keyboard)
        ]


black =
    Css.rgb 0 0 0


white =
    Css.rgb 255 255 255


viewBody model =
    [ Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.height (Css.pct 100)
            ]
        ]
        [ Css.Global.global
            [ Css.Global.html
                [ Css.height (Css.pct 100)
                , Css.overflow Css.hidden
                ]
            , Css.Global.body
                [ Css.height (Css.pct 100)
                , Css.margin Css.zero
                , Css.backgroundColor black
                , Css.color white
                ]
            ]
        , viewDebugInfo model
        , viewContents model
        ]
        |> Html.Styled.toUnstyled
    ]


viewDebugInfo { framerate, keyboard } =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.position Css.absolute
            , Css.left Css.zero
            , Css.top Css.zero
            , Css.whiteSpace Css.preWrap
            ]
        ]
        [ Html.Styled.text
            ("FPS: "
                ++ Framerate.toString 1 (Framerate.framerate framerate)
            )
        ]


viewContents { screen, keyboard } =
    case screen of
        Title ->
            Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.textAlign Css.center
                    , Css.height (Css.pct 100)
                    ]
                , Html.Styled.Events.onClick StartGame
                ]
                [ Html.Styled.div
                    [ Html.Styled.Attributes.css
                        [ Css.marginTop (Css.vh 40)
                        , Css.fontSize Css.xxLarge
                        ]
                    ]
                    [ Html.Styled.text "Ants"
                    ]
                , Html.Styled.div
                    [ Html.Styled.Attributes.css
                        [ Css.fontSize Css.small
                        ]
                    ]
                    [ Html.Styled.text "(click anywhere to start)"
                    ]
                ]

        Gameplay gameplay ->
            viewGameplay keyboard gameplay


viewGameplay keyboard ({ world } as gameplay) =
    let
        allEntities =
            World.allEntities world

        ( viewportFrame, viewportLength ) =
            viewport keyboard gameplay

        viewBoxRadius =
            384

        scale =
            viewBoxRadius / Length.inCssPixels viewportLength
    in
    Svg.Styled.svg
        [ Svg.Styled.Attributes.css
            [ Css.width (Css.pct 100)
            , Css.height (Css.pct 100)
            ]
        , Svg.Styled.Attributes.viewBox (String.join " " (List.map String.fromInt [ -viewBoxRadius, -viewBoxRadius, 2 * viewBoxRadius, 2 * viewBoxRadius ]))
        ]
        [ Svg.Styled.g
            [ Svg.Styled.Attributes.transform (placeWorldInViewport viewportFrame scale).value
            ]
            (allEntities
                |> List.map
                    (\{ glyph, frame } ->
                        Svg.Styled.g
                            [ Svg.Styled.Attributes.transform (placeEntityInWorld frame).value ]
                            [ viewEntity glyph
                            ]
                    )
            )
        ]


{-| Creates a css (or svg) transformation matrix which will put the image in the given frame.
I used the source of ianmackenzie/elm-geometry-svg:Geometry.Svg.placeIn as a reference for calculating this matrix
-}
placeEntityInWorld : Frame2d.Frame2d Length.Meters World.WorldCoordinates { defines : World.EntityCoordinates } -> Css.Transform {}
placeEntityInWorld frame =
    let
        px =
            Point2d.xCoordinate (Frame2d.originPoint frame)
                |> Length.inCssPixels

        py =
            Point2d.yCoordinate (Frame2d.originPoint frame)
                |> Length.inCssPixels

        d1 =
            Direction2d.unwrap (Frame2d.xDirection frame)

        d2 =
            Direction2d.unwrap (Frame2d.yDirection frame)
    in
    Css.matrix d1.x d1.y d2.x d2.y px py


{-| Creates a css (or svg) transformation matrix which will put the image in the given frame.
I used the source of ianmackenzie/elm-geometry-svg:Geometry.Svg.placeIn as a reference for calculating this matrix
-}
placeWorldInViewport : Frame2d.Frame2d Length.Meters World.WorldCoordinates { defines : ViewportCoordinates } -> Float -> Css.Transform {}
placeWorldInViewport frame scale =
    let
        globalFrameRelative =
            Frame2d.relativeTo frame Frame2d.atOrigin

        px =
            Point2d.xCoordinate (Frame2d.originPoint globalFrameRelative)
                |> Length.inCssPixels

        py =
            Point2d.yCoordinate (Frame2d.originPoint globalFrameRelative)
                |> Length.inCssPixels

        d1 =
            Direction2d.unwrap (Frame2d.xDirection globalFrameRelative)

        d2 =
            Direction2d.unwrap (Frame2d.yDirection globalFrameRelative)
    in
    Css.matrix (d1.x * scale) (-d1.y * scale) (d2.x * scale) (-d2.y * scale) (px * scale) (-py * scale)


viewEntity glyph =
    case glyph of
        World.Anthill ->
            Svg.Styled.circle
                [ Svg.Styled.Attributes.css
                    [ Css.Extra.r (Css.cm 1.75)
                    , Css.Extra.stroke (Css.rgb 0 0 255)
                    ]
                ]
                []

        World.Ant ->
            viewGlyphAnt (Css.rgb 255 255 255)

        World.PlayerAnt ->
            viewGlyphAnt (Css.rgb 255 0 0)


viewGlyphAnt color =
    Svg.Styled.g []
        [ Svg.Styled.circle
            [ Svg.Styled.Attributes.css
                [ Css.Extra.r (Css.mm 1)
                , Css.Extra.cy (Css.mm 2)
                , Css.Extra.stroke color
                ]
            ]
            []
        , Svg.Styled.circle
            [ Svg.Styled.Attributes.css
                [ Css.Extra.r (Css.mm 1)
                , Css.Extra.stroke color
                ]
            ]
            []
        , Svg.Styled.circle
            [ Svg.Styled.Attributes.css
                [ Css.Extra.r (Css.mm 1)
                , Css.Extra.cy (Css.mm -2)
                , Css.Extra.stroke color
                ]
            ]
            []
        ]


viewport : Keyboard.State -> { a | world : World.World } -> ( Frame2d.Frame2d Length.Meters World.WorldCoordinates { defines : ViewportCoordinates }, Length.Length )
viewport keyboard { world } =
    if Keyboard.isPressed Keyboard.KeyCode.space keyboard then
        globalFrame world

    else
        case World.playerFrame world of
            Nothing ->
                globalFrame world

            Just playerFrame ->
                ( playerFrame
                    |> Frame2d.copy
                    |> Frame2d.translateAlongOwn Frame2d.yAxis (Length.centimeters 2)
                , Length.centimeters 3
                )


globalFrame world =
    let
        allEntities =
            World.allEntities world

        globalFrameMargin =
            Length.centimeters 3

        minX =
            allEntities
                |> List.map (\{ frame } -> Point2d.xCoordinate (Frame2d.originPoint frame))
                |> Quantity.minimum
                |> Maybe.withDefault (Quantity.Quantity 0)
                |> Quantity.minus globalFrameMargin

        maxX =
            allEntities
                |> List.map (\{ frame } -> Point2d.xCoordinate (Frame2d.originPoint frame))
                |> Quantity.maximum
                |> Maybe.withDefault (Quantity.Quantity 0)
                |> Quantity.plus globalFrameMargin

        minY =
            allEntities
                |> List.map (\{ frame } -> Point2d.yCoordinate (Frame2d.originPoint frame))
                |> Quantity.minimum
                |> Maybe.withDefault (Quantity.Quantity 0)
                |> Quantity.minus globalFrameMargin

        maxY =
            allEntities
                |> List.map (\{ frame } -> Point2d.yCoordinate (Frame2d.originPoint frame))
                |> Quantity.maximum
                |> Maybe.withDefault (Quantity.Quantity 0)
                |> Quantity.plus globalFrameMargin
    in
    ( Frame2d.atPoint (Point2d.midpoint (Point2d.xy minX minY) (Point2d.xy maxX maxY))
    , Quantity.half (Quantity.max (Quantity.minus minX maxX) (Quantity.minus minY maxY))
    )
