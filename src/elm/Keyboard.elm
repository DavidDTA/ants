module Keyboard exposing (Msg, State, event, init, update)

import Json.Decode
import Set exposing (Set)


type Msg
    = KeyUp { code : String }
    | KeyDown { code : String }


type State
    = Keyboard
        { pressed : Set String
        }


init : State
init =
    Keyboard { pressed = Set.empty }


update : Msg -> State -> State
update msg (Keyboard { pressed }) =
    case msg of
        KeyUp { code } ->
            Keyboard { pressed = Set.remove code pressed }

        KeyDown { code } ->
            Keyboard { pressed = Set.insert code pressed }


event : (Msg -> msg) -> Json.Decode.Decoder msg
event tag =
    Json.Decode.at [ "type" ] Json.Decode.string
        |> Json.Decode.andThen
            (\type_ ->
                case type_ of
                    "keydown" ->
                        Json.Decode.succeed KeyDown

                    "keyup" ->
                        Json.Decode.succeed KeyUp

                    _ ->
                        Json.Decode.fail ("type: \"" ++ type_ ++ "\"")
            )
        |> Json.Decode.andThen
            (\msg ->
                Json.Decode.at [ "code" ] Json.Decode.string
                    |> Json.Decode.map (\code -> msg { code = code })
            )
        |> Json.Decode.map tag
