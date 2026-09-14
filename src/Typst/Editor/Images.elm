module Typst.Editor.Images exposing (attachments, isTooLarge)

import Dict exposing (Dict)


maxBase64Length : Int
maxBase64Length =
    2 * 1024 * 1024


isTooLarge : String -> Bool
isTooLarge data =
    String.length data > maxBase64Length


referencedIds : String -> List String
referencedIds source =
    String.split "#image(\"" source
        |> List.drop 1
        |> List.filterMap
            (\chunk ->
                case String.split "\"" chunk of
                    first :: _ ->
                        if String.endsWith ".png" first then
                            Just (String.dropRight 4 first)

                        else
                            Nothing

                    [] ->
                        Nothing
            )


attachments : Dict String String -> Dict String String -> String -> List ( String, String )
attachments knownImages pendingImages source =
    let
        allImages =
            Dict.union pendingImages knownImages
    in
    referencedIds source
        |> List.filterMap (\imgId -> Dict.get imgId allImages |> Maybe.map (\data -> ( imgId ++ ".png", data )))
