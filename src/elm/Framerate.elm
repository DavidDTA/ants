module Framerate exposing (Framerate, framerate, init, initWithBufferSize, toString, update)

import Array
import Duration
import Quantity
import Time


type Frames
    = Frames


type Framerate
    = Framerate
        { timestamps : Array.Array Time.Posix
        , nextIndex : Int
        , processedFrames : Int
        }


type alias FramesPerSecond =
    Quantity.Quantity Float (Quantity.Rate Frames Duration.Seconds)


init : Framerate
init =
    initWithBufferSize 65


initWithBufferSize : Int -> Framerate
initWithBufferSize bufferSize =
    Framerate
        { timestamps = Array.repeat bufferSize epoch
        , nextIndex = 0
        , processedFrames = 0
        }


update : Time.Posix -> Framerate -> Framerate
update newFrame (Framerate { timestamps, nextIndex, processedFrames }) =
    let
        arrayLength =
            Array.length timestamps
    in
    Framerate
        { timestamps = Array.set nextIndex newFrame timestamps
        , nextIndex = modBy arrayLength (nextIndex + 1)
        , processedFrames = min (processedFrames + 1) arrayLength
        }


framerate : Framerate -> FramesPerSecond
framerate (Framerate { timestamps, nextIndex, processedFrames }) =
    let
        getTimestampAtCircularIndex index =
            Array.get (modBy (Array.length timestamps) index) timestamps |> Maybe.withDefault epoch
    in
    Quantity.per
        (Duration.from
            (getTimestampAtCircularIndex (nextIndex - processedFrames))
            (getTimestampAtCircularIndex (nextIndex - 1))
        )
        (Quantity.toFloatQuantity (Quantity.Quantity (processedFrames - 1)))


toString : Int -> FramesPerSecond -> String
toString decimalPlaces (Quantity.Quantity fps) =
    let
        factor =
            10 ^ decimalPlaces

        rounded =
            round (fps * toFloat factor)

        head =
            rounded // factor

        dec =
            modBy factor rounded
    in
    String.fromInt head
        ++ (if decimalPlaces > 0 then
                "." ++ String.fromInt dec

            else
                ""
           )


epoch =
    Time.millisToPosix 0
