module Pages.Stats exposing (Model, Msg, init, requestSummary, update, view)

import Html exposing (Html, div, h3, option, p, select, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)
import Ops.OpsLog exposing (OpsLog)
import Pages.Stats.Charts as Charts
import Pages.Stats.Data as Data exposing (Forecast, History, Summary)
import Sea.Sea exposing (Sea)
import Task
import Time


type Model
    = Loading
    | Loaded LoadedState


type alias LoadedState =
    { summary : Summary
    , historyDays : Int
    , history : History
    , forecastDays : Int
    , forecast : Forecast
    }


defaultHistoryDays : Int
defaultHistoryDays =
    30


defaultForecastDays : Int
defaultForecastDays =
    30


init : ( Model, Cmd Msg )
init =
    ( Loading, requestSummary )


requestSummary : Cmd Msg
requestSummary =
    Task.perform GotTimeForSummary Time.now


type Msg
    = GotTimeForSummary Time.Posix
    | HistoryDaysChanged Int
    | GotTimeForHistory Int Time.Posix
    | ForecastDaysChanged Int
    | GotTimeForForecast Int Time.Posix


update : Int -> OpsLog -> Sea -> Msg -> Model -> ( Model, Cmd Msg )
update dailyNewLimit opsLog sea msg model =
    case msg of
        GotTimeForSummary now ->
            ( Loaded
                { summary = Data.summarize dailyNewLimit now opsLog sea
                , historyDays = defaultHistoryDays
                , history = Data.history now defaultHistoryDays opsLog
                , forecastDays = defaultForecastDays
                , forecast = Data.forecast defaultForecastDays now opsLog sea
                }
            , Cmd.none
            )

        HistoryDaysChanged days ->
            ( model, Task.perform (GotTimeForHistory days) Time.now )

        GotTimeForHistory days now ->
            case model of
                Loaded state ->
                    ( Loaded { state | historyDays = days, history = Data.history now days opsLog }, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

        ForecastDaysChanged days ->
            ( model, Task.perform (GotTimeForForecast days) Time.now )

        GotTimeForForecast days now ->
            case model of
                Loaded state ->
                    ( Loaded { state | forecastDays = days, forecast = Data.forecast days now opsLog sea }, Cmd.none )

                Loading ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            p [ class "stats-loading" ] [ text "Loading..." ]

        Loaded state ->
            let
                totals =
                    Data.retentionTotals state.history
            in
            div []
                [ h3 [ class "history-heading" ] [ text "General" ]
                , div [ class "stats-grid" ]
                    [ statTile (String.fromInt state.summary.total) "Total cards"
                    , statTile (String.fromInt state.summary.due) "Due now"
                    , statTile (String.fromInt state.summary.learning) "Learning"
                    , statTile (String.fromInt state.summary.new) "New"
                    , statTile (dailyStreakLabel state.summary.dailyStreak) "Daily streak"
                    , statTile (retainedLabel state.summary.retainedPercent) "Retained now"
                    ]
                , div [ class "history-heading-row" ]
                    [ h3 [ class "history-heading" ] [ text "Reviews" ]
                    , viewDaysSelect state.historyDays
                    ]
                , div [ class "history-card" ]
                    [ Charts.viewHistoryChart state.history
                    , Charts.viewHistoryLegend
                    ]
                , div [ class "history-heading-row" ]
                    [ h3 [ class "history-heading" ] [ text "Forecast" ]
                    , viewForecastDaysSelect state.forecastDays
                    ]
                , div [ class "history-card" ]
                    [ Charts.viewForecastChart state.forecast ]
                , h3 [ class "history-heading retention-heading" ] [ text "Retention" ]
                , div [ class "history-card" ]
                    [ div [ class "retention-row" ]
                        [ div [ class "retention-chart-wrap" ] [ Charts.viewRetentionDonut totals ]
                        , Charts.viewRetentionLegend totals
                        ]
                    ]
                ]


dailyStreakLabel : Int -> String
dailyStreakLabel streak =
    String.fromInt streak
        ++ (if streak == 1 then
                " day"

            else
                " days"
           )


retainedLabel : Maybe Int -> String
retainedLabel maybePercent =
    case maybePercent of
        Just percent ->
            String.fromInt percent ++ "%"

        Nothing ->
            "—"


statTile : String -> String -> Html msg
statTile val label =
    div [ class "stat-tile" ]
        [ div [ class "stat-value" ] [ text val ]
        , div [ class "stat-label" ] [ text label ]
        ]


viewDaysSelect : Int -> Html Msg
viewDaysSelect selectedDays =
    select
        [ class "link-select"
        , onInput (String.toInt >> Maybe.withDefault selectedDays >> HistoryDaysChanged)
        ]
        (List.map (daysOption selectedDays) [ 7, 30, 90, 365 ])


daysOption : Int -> Int -> Html Msg
daysOption selectedDays days =
    option [ value (String.fromInt days), selected (days == selectedDays) ]
        [ text (daysLabel days) ]


daysLabel : Int -> String
daysLabel days =
    case days of
        7 ->
            "Last 7 days"

        30 ->
            "Last 30 days"

        90 ->
            "Last 90 days"

        365 ->
            "Last year"

        _ ->
            String.fromInt days ++ " days"


viewForecastDaysSelect : Int -> Html Msg
viewForecastDaysSelect selectedDays =
    select
        [ class "link-select"
        , onInput (String.toInt >> Maybe.withDefault selectedDays >> ForecastDaysChanged)
        ]
        (List.map (forecastDaysOption selectedDays) [ 7, 30, 90, 365 ])


forecastDaysOption : Int -> Int -> Html Msg
forecastDaysOption selectedDays days =
    option [ value (String.fromInt days), selected (days == selectedDays) ]
        [ text (forecastDaysLabel days) ]


forecastDaysLabel : Int -> String
forecastDaysLabel days =
    case days of
        7 ->
            "Next 7 days"

        30 ->
            "Next 30 days"

        90 ->
            "Next 90 days"

        365 ->
            "Next year"

        _ ->
            "Next " ++ String.fromInt days ++ " days"
