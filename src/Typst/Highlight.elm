module Typst.Highlight exposing (Node, decoder, view)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode


type Node
    = Leaf (Maybe String) String
    | Branch (Maybe String) (List Node)


decoder : Decode.Decoder Node
decoder =
    Decode.map3 (\tag maybeText children -> ( tag, maybeText, children ))
        (Decode.field "tag" (Decode.nullable Decode.string))
        (Decode.field "text" (Decode.nullable Decode.string))
        (Decode.field "children" (Decode.list (Decode.lazy (\_ -> decoder))))
        |> Decode.map
            (\( tag, maybeText, children ) ->
                case maybeText of
                    Just leafText ->
                        Leaf tag leafText

                    Nothing ->
                        Branch tag children
            )


view : Node -> Html msg
view node =
    case node of
        Leaf (Just cls) leafText ->
            span [ class cls ] [ text leafText ]

        Leaf Nothing leafText ->
            text leafText

        Branch tag children ->
            let
                rendered =
                    List.map view children
            in
            case tag of
                Just cls ->
                    span [ class cls ] rendered

                Nothing ->
                    span [] rendered
