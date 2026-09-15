module Pages.Stats.Charts exposing
    ( viewForecastChart
    , viewHistoryChart
    , viewHistoryLegend
    , viewRetentionDonut
    , viewRetentionLegend
    )

import Date exposing (Date)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Pages.Stats.Data exposing (DayCounts, Forecast, History, RatingTotals)
import Svg
import Svg.Attributes as SA


ratingSegments : List ( DayCounts -> Int, String, String )
ratingSegments =
    [ ( .easy, "hist-bar-easy", "Easy" )
    , ( .good, "hist-bar-good", "Good" )
    , ( .hard, "hist-bar-hard", "Hard" )
    , ( .again, "hist-bar-again", "Again" )
    ]


viewHistoryChart : History -> Html msg
viewHistoryChart hist =
    let
        chartWidth =
            600

        chartHeight =
            140

        plotHeight =
            chartHeight - 16

        n =
            List.length hist

        slot =
            chartWidth / toFloat (max 1 n)

        barGap =
            min 2 (slot * 0.2)

        barWidth =
            max 0.5 (slot - barGap)

        maxTotal =
            hist
                |> List.map (\d -> toFloat (d.counts.again + d.counts.hard + d.counts.good + d.counts.easy))
                |> List.maximum
                |> Maybe.withDefault 0
                |> max 1

        labelEvery =
            max 1 (ceiling (toFloat n / 6))

        barsFor i day =
            let
                x =
                    toFloat i * (barWidth + barGap)

                place ( getter, cls, label ) ( yTop, acc ) =
                    let
                        count =
                            getter day.counts
                    in
                    if count == 0 then
                        ( yTop, acc )

                    else
                        let
                            segHeight =
                                (toFloat count / maxTotal) * plotHeight

                            newY =
                                yTop - segHeight

                            rect =
                                Svg.rect
                                    [ SA.class cls
                                    , SA.x (numAttr x)
                                    , SA.y (numAttr newY)
                                    , SA.width (numAttr barWidth)
                                    , SA.height (numAttr segHeight)
                                    ]
                                    [ Svg.title []
                                        [ text (formatMonthDay day.date ++ ": " ++ String.fromInt count ++ " " ++ String.toLower label) ]
                                    ]
                        in
                        ( newY, rect :: acc )
            in
            Tuple.second (List.foldl place ( plotHeight, [] ) ratingSegments)

        bars =
            List.indexedMap barsFor hist |> List.concat

        labels =
            hist
                |> List.indexedMap Tuple.pair
                |> List.filter (\( i, _ ) -> modBy labelEvery i == 0)
                |> List.map
                    (\( i, day ) ->
                        Svg.text_
                            [ SA.class "hist-axis-label"
                            , SA.x (numAttr (toFloat i * (barWidth + barGap) + barWidth / 2))
                            , SA.y (numAttr (chartHeight - 3))
                            , SA.textAnchor "middle"
                            ]
                            [ text (formatMonthDay day.date) ]
                    )
    in
    Svg.svg
        [ SA.class "history-chart"
        , SA.viewBox ("0 0 " ++ numAttr chartWidth ++ " " ++ numAttr chartHeight)
        ]
        (bars ++ labels)


viewHistoryLegend : Html msg
viewHistoryLegend =
    Html.div [ class "history-legend" ]
        (List.map
            (\( _, cls, label ) ->
                span [ class "history-legend-item" ]
                    [ span [ class ("history-swatch " ++ cls) ] []
                    , span [] [ text label ]
                    ]
            )
            ratingSegments
        )


viewForecastChart : Forecast -> Html msg
viewForecastChart fc =
    let
        chartWidth =
            600

        chartHeight =
            140

        plotHeight =
            chartHeight - 16

        n =
            List.length fc

        slot =
            chartWidth / toFloat (max 1 n)

        barGap =
            min 2 (slot * 0.2)

        barWidth =
            max 0.5 (slot - barGap)

        maxCount =
            fc
                |> List.map .count
                |> List.maximum
                |> Maybe.withDefault 0
                |> max 1
                |> toFloat

        labelEvery =
            max 1 (ceiling (toFloat n / 6))

        barFor i day =
            if day.count == 0 then
                Nothing

            else
                let
                    x =
                        toFloat i * (barWidth + barGap)

                    barHeight =
                        (toFloat day.count / maxCount) * plotHeight
                in
                Just
                    (Svg.rect
                        [ SA.class "forecast-bar"
                        , SA.x (numAttr x)
                        , SA.y (numAttr (plotHeight - barHeight))
                        , SA.width (numAttr barWidth)
                        , SA.height (numAttr barHeight)
                        ]
                        [ Svg.title []
                            [ text
                                (formatMonthDay day.date
                                    ++ ": "
                                    ++ String.fromInt day.count
                                    ++ (if day.count == 1 then
                                            " card due"

                                        else
                                            " cards due"
                                       )
                                )
                            ]
                        ]
                    )

        bars =
            List.indexedMap barFor fc |> List.filterMap identity

        labels =
            fc
                |> List.indexedMap Tuple.pair
                |> List.filter (\( i, _ ) -> modBy labelEvery i == 0)
                |> List.map
                    (\( i, day ) ->
                        Svg.text_
                            [ SA.class "hist-axis-label"
                            , SA.x (numAttr (toFloat i * (barWidth + barGap) + barWidth / 2))
                            , SA.y (numAttr (chartHeight - 3))
                            , SA.textAnchor "middle"
                            ]
                            [ text (formatMonthDay day.date) ]
                    )
    in
    Svg.svg
        [ SA.class "history-chart"
        , SA.viewBox ("0 0 " ++ numAttr chartWidth ++ " " ++ numAttr chartHeight)
        ]
        (bars ++ labels)


viewRetentionDonut : RatingTotals -> Html msg
viewRetentionDonut totals =
    let
        size =
            120

        r =
            50

        c =
            size / 2
    in
    if totals.total == 0 then
        Svg.svg [ SA.class "retention-donut", SA.viewBox (numAttr 0 ++ " 0 " ++ numAttr size ++ " " ++ numAttr size) ]
            [ Svg.circle
                [ SA.class "retention-empty", SA.cx (numAttr c), SA.cy (numAttr c), SA.r (numAttr r) ]
                [ Svg.title [] [ text "No reviews yet" ] ]
            ]

    else
        let
            wedgeFor ( label, count, cls ) ( angleSoFar, acc ) =
                if count == 0 then
                    ( angleSoFar, acc )

                else
                    let
                        nextAngle =
                            angleSoFar + (toFloat count / toFloat totals.total) * 360

                        percent =
                            round (toFloat count / toFloat totals.total * 100)

                        wedge =
                            Svg.path
                                [ SA.class cls, SA.d (pieWedgePath c c r angleSoFar nextAngle) ]
                                [ Svg.title [] [ text (label ++ ": " ++ String.fromInt count ++ " (" ++ String.fromInt percent ++ "%)") ] ]
                    in
                    ( nextAngle, wedge :: acc )

            wedges =
                List.foldl wedgeFor
                    ( 0, [] )
                    [ ( "Again", totals.again, "hist-bar-again" )
                    , ( "Hard", totals.hard, "hist-bar-hard" )
                    , ( "Good", totals.good, "hist-bar-good" )
                    , ( "Easy", totals.easy, "hist-bar-easy" )
                    ]
                    |> Tuple.second
                    |> List.reverse
        in
        Svg.svg [ SA.class "retention-donut", SA.viewBox (numAttr 0 ++ " 0 " ++ numAttr size ++ " " ++ numAttr size) ] wedges


viewRetentionLegend : RatingTotals -> Html msg
viewRetentionLegend totals =
    let
        entries =
            [ ( "Again", totals.again, "hist-bar-again" )
            , ( "Hard", totals.hard, "hist-bar-hard" )
            , ( "Good", totals.good, "hist-bar-good" )
            , ( "Easy", totals.easy, "hist-bar-easy" )
            ]
    in
    Html.div [ class "retention-legend" ]
        (List.map
            (\( label, count, cls ) ->
                span [ class "history-legend-item" ]
                    [ span [ class ("history-swatch " ++ cls) ] []
                    , span []
                        [ text
                            (label
                                ++ (if totals.total > 0 then
                                        ": " ++ String.fromInt (round (toFloat count / toFloat totals.total * 100)) ++ "%"

                                    else
                                        ""
                                   )
                            )
                        ]
                    ]
            )
            entries
        )


polarPoint : Float -> Float -> Float -> Float -> ( Float, Float )
polarPoint cx cy r angleDeg =
    let
        rad =
            (angleDeg - 90) * pi / 180
    in
    ( cx + r * cos rad, cy + r * sin rad )


pieWedgePath : Float -> Float -> Float -> Float -> Float -> String
pieWedgePath cx cy r startAngle endAngle =
    if endAngle - startAngle >= 360 then
        "M " ++ numAttr cx ++ " " ++ numAttr (cy - r) ++ " A " ++ numAttr r ++ " " ++ numAttr r ++ " 0 1 1 " ++ numAttr (cx - 0.01) ++ " " ++ numAttr (cy - r) ++ " Z"

    else
        let
            ( sx, sy ) =
                polarPoint cx cy r startAngle

            ( ex, ey ) =
                polarPoint cx cy r endAngle

            largeArc =
                if endAngle - startAngle > 180 then
                    "1"

                else
                    "0"
        in
        "M " ++ numAttr cx ++ " " ++ numAttr cy ++ " L " ++ numAttr sx ++ " " ++ numAttr sy ++ " A " ++ numAttr r ++ " " ++ numAttr r ++ " 0 " ++ largeArc ++ " 1 " ++ numAttr ex ++ " " ++ numAttr ey ++ " Z"


formatMonthDay : Date -> String
formatMonthDay date =
    padInt (Date.day date) ++ "/" ++ padInt (Date.monthNumber date)


padInt : Int -> String
padInt n =
    String.padLeft 2 '0' (String.fromInt n)


numAttr : Float -> String
numAttr =
    String.fromFloat
