module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, li, span, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Task
import Time exposing (Posix, Zone)
import Time.Extra as TimeExtra exposing (Interval(..))


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 16 TimeChange



-- MODEL


type alias Model =
    { zone : Zone
    , startTimestamp : Posix
    , laps : List Posix
    , isTiming : Bool
    , currentTimestamp : Posix
    , durationOnStop : Posix
    , lapDurationOnStop : Posix
    , lastLapTimestamp : Posix
    }


initialTime : Posix
initialTime =
    Time.millisToPosix 0


initialModel : Model
initialModel =
    { zone = Time.utc
    , startTimestamp = initialTime
    , laps = []
    , isTiming = False
    , currentTimestamp = initialTime
    , durationOnStop = initialTime
    , lapDurationOnStop = initialTime
    , lastLapTimestamp = initialTime
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Task.perform TimeZoneChange Time.here
    )



-- UPDATE


type Msg
    = ButtonPress UserAction
    | ReceiveTimestamp UserAction Posix
    | TimeChange Posix
    | TimeZoneChange Zone


type UserAction
    = Start
    | Stop
    | Lap
    | Reset


userActionToString : UserAction -> String
userActionToString userAction =
    case userAction of
        Start ->
            "Start"

        Stop ->
            "Stop"

        Lap ->
            "Lap"

        Reset ->
            "Reset"


update : Msg -> Model -> ( Model, Cmd Msg )
update change model =
    case change of
        ButtonPress userAction ->
            ( model
            , Task.perform (ReceiveTimestamp userAction) Time.now
            )

        ReceiveTimestamp userAction time ->
            case userAction of
                Start ->
                    ( handleStart time model
                    , Cmd.none
                    )

                Stop ->
                    ( handleStop time model
                    , Cmd.none
                    )

                Lap ->
                    ( handleLap time model
                    , Cmd.none
                    )

                Reset ->
                    init

        TimeChange time ->
            if model.isTiming then
                ( { model | currentTimestamp = time }
                , Cmd.none
                )

            else
                ( model
                , Cmd.none
                )

        TimeZoneChange zone ->
            ( { model | zone = zone }
            , Cmd.none
            )


handleStart : Posix -> Model -> Model
handleStart time model =
    { model
        | isTiming = True
        , startTimestamp = time
        , lastLapTimestamp = time
    }


handleStop : Posix -> Model -> Model
handleStop time model =
    let
        duration =
            Time.posixToMillis model.durationOnStop + Time.posixToMillis time - Time.posixToMillis model.startTimestamp
    in
    { model
        | isTiming = False
        , durationOnStop = Time.millisToPosix duration
        , lapDurationOnStop = Time.millisToPosix (duration - total model.laps)
    }


handleLap : Posix -> Model -> Model
handleLap time model =
    let
        lapDurationOnStop =
            Time.posixToMillis model.lapDurationOnStop

        timeAsMillis =
            Time.posixToMillis time

        lastLapTimestamp =
            Time.posixToMillis model.lastLapTimestamp
    in
    { model
        | laps = Time.millisToPosix (lapDurationOnStop + timeAsMillis - lastLapTimestamp) :: model.laps
        , lastLapTimestamp = time
        , lapDurationOnStop = initialTime
    }


total : List Posix -> Int
total laps =
    laps
        |> List.map Time.posixToMillis
        |> List.foldl (+) 0


posixToString : Zone -> Posix -> String
posixToString zone posix =
    let
        posixParts =
            TimeExtra.posixToParts zone posix

        minutes =
            posixParts.minute

        seconds =
            posixParts.second

        centiseconds =
            posixParts.millisecond // 10

        pad n =
            n
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    pad minutes ++ ":" ++ pad seconds ++ "." ++ pad centiseconds



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ div
            [ class "header" ]
            [ text "Chronographify" ]
        , timers model
        , buttonRow model.isTiming
        , lapsList model.zone model.laps
        ]


timers : Model -> Html msg
timers model =
    let
        timeDiff =
            Time.posixToMillis model.currentTimestamp - Time.posixToMillis model.startTimestamp

        durationOnStop =
            Time.posixToMillis model.durationOnStop

        currentDuration =
            if model.isTiming then
                durationOnStop + timeDiff

            else
                durationOnStop

        lapDuration =
            currentDuration - total model.laps

        durationToText duration =
            duration
                |> Time.millisToPosix
                |> posixToString model.zone
                |> text
    in
    div
        [ class "timersWrapper" ]
        [ div
            [ class "timers" ]
            [ div
                [ class "lapTimer" ]
                [ durationToText lapDuration ]
            , div
                [ class "totalTimer" ]
                [ durationToText currentDuration ]
            ]
        ]


buttonRow : Bool -> Html Msg
buttonRow isTiming =
    div
        [ class "buttonRow" ]
    <|
        if isTiming then
            [ actionButton Stop
            , actionButton Lap
            ]

        else
            [ actionButton Start
            , actionButton Reset
            ]


lapsList : Zone -> List Posix -> Html msg
lapsList zone laps =
    ul
        [ class "laps" ]
        (laps
            |> List.reverse
            |> List.indexedMap (lapEntry zone)
            |> List.reverse
        )


actionButton : UserAction -> Html Msg
actionButton action =
    button
        [ onClick <| ButtonPress action
        , actionButtonClasses action
        ]
        [ text <| userActionToString action ]


lapEntry : Zone -> Int -> Posix -> Html msg
lapEntry zone index entry =
    let
        lapNumber =
            String.fromInt (index + 1) ++ "."
    in
    li
        [ class "lapEntry" ]
        [ span
            [ class "lapNumber" ]
            [ text lapNumber ]
        , span
            [ class "lapTime" ]
            [ text <| posixToString zone entry ]
        ]


actionButtonClasses : UserAction -> Attribute msg
actionButtonClasses action =
    let
        buttonColorClass =
            case action of
                Start ->
                    "startButton"

                Stop ->
                    "stopButton"

                Lap ->
                    "resetAndLapButton"

                Reset ->
                    "resetAndLapButton"
    in
    class ("button " ++ buttonColorClass)
