module Typst.Editor.OutsideClick exposing (decoder)

import Json.Decode as Decode


decoder : String -> msg -> Decode.Decoder msg
decoder containerId msg =
    Decode.field "target" (isOutsideOf containerId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed msg

                else
                    Decode.fail "click was inside the container"
            )


isOutsideOf : String -> Decode.Decoder Bool
isOutsideOf containerId =
    Decode.field "id" Decode.string
        |> Decode.andThen
            (\nodeId ->
                if nodeId == containerId then
                    Decode.succeed False

                else
                    Decode.oneOf
                        [ Decode.field "parentNode" (Decode.lazy (\_ -> isOutsideOf containerId))
                        , Decode.succeed True
                        ]
            )
