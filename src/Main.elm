module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, li, span, text, ul)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Task
import Time exposing (Posix)
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
    { zone : Time.Zone
    , startTimestamp : Posix
    , laps : List Posix
    , isTiming : Bool
    , currentTimestamp : Posix
    , durationOnStop : Posix
    , lapDurationOnStop : Posix
    , lastLapTimestamp : Posix
    }


initialModel : Model
initialModel =
    { zone = Time.utc
    , startTimestamp = Time.millisToPosix 0
    , laps = []
    , isTiming = False
    , currentTimestamp = Time.millisToPosix 0
    , durationOnStop = Time.millisToPosix 0
    , lapDurationOnStop = Time.millisToPosix 0
    , lastLapTimestamp = Time.millisToPosix 0
    }


init : ( Model, Cmd a )
init =
    ( initialModel
    , Cmd.none
    )



-- UPDATE


type Msg
    = ReceiveTimestamp UserAction Posix
    | TimeChange Posix
    | ButtonPress UserAction


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
    { model
        | laps = Time.millisToPosix (Time.posixToMillis model.lapDurationOnStop + Time.posixToMillis time - Time.posixToMillis model.lastLapTimestamp) :: model.laps
        , lastLapTimestamp = time
        , lapDurationOnStop = Time.millisToPosix 0
    }


total : List Posix -> Int
total laps =
    laps
        |> List.map Time.posixToMillis
        |> List.foldl (+) 0


millisToString : Time.Zone -> Posix -> String
millisToString zone posix =
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
            headerStyles
            [ text "Chronographify" ]
        , timers model
        , buttonRow model.isTiming
        , lapsList model.zone model.laps
        ]


timers : Model -> Html Msg
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
    in
    div
        timersWrapperStyles
        [ div
            timersStyles
            [ div
                lapTimerStyles
                [ text <| millisToString model.zone <| Time.millisToPosix lapDuration ]
            , div
                totalTimerStyles
                [ text <| millisToString model.zone <| Time.millisToPosix currentDuration ]
            ]
        ]


buttonRow : Bool -> Html Msg
buttonRow isTiming =
    div
        buttonRowStyles
    <|
        if isTiming then
            [ actionButton Stop
            , actionButton Lap
            ]

        else
            [ actionButton Start
            , actionButton Reset
            ]


lapsList : Time.Zone -> List Posix -> Html Msg
lapsList zone laps =
    ul
        lapsStyles
        (laps
            |> List.reverse
            |> List.indexedMap (lapEntry zone)
            |> List.reverse
        )


actionButton : UserAction -> Html Msg
actionButton action =
    button
        ((onClick <| ButtonPress action) :: actionButtonStyles action)
        [ text <| userActionToString action ]


lapEntry : Time.Zone -> Int -> Posix -> Html Msg
lapEntry zone index entry =
    let
        lapNumber =
            String.fromInt (index + 1) ++ "."
    in
    li
        lapEntryStyles
        [ span
            lapNumberStyles
            [ text lapNumber ]
        , span
            lapTimeStyles
            [ text <| millisToString zone entry ]
        ]



-- COLORS


offBlack : String
offBlack =
    "#333"


offWhite : String
offWhite =
    "#F5F5F5"


green : String
green =
    "#5CEC3D"


red : String
red =
    "#EC483D"


lightGrey : String
lightGrey =
    "#CCC"


midGrey : String
midGrey =
    "#777"



-- STYLES


buttonBackground : UserAction -> String
buttonBackground action =
    case action of
        Start ->
            green

        Stop ->
            red

        Lap ->
            lightGrey

        Reset ->
            lightGrey


actionButtonStyles : UserAction -> List (Attribute msg)
actionButtonStyles action =
    [ style "flex-grow" "1"
    , style "min-width" "50%"
    , style "font-size" "1.2rem"
    , style "border" "none"
    , style "outline" "none"
    , style "padding" "0.4rem"
    , style "background" <| buttonBackground action
    ]


buttonRowStyles : List (Attribute msg)
buttonRowStyles =
    [ style "display" "flex"
    , style "flex-direction" "row"
    ]


headerStyles : List (Attribute msg)
headerStyles =
    [ style "text-transform" "uppercase"
    , style "background" offBlack
    , style "padding" "0.5em 1rem"
    , style "font-size" "1.2rem"
    , style "color" "#f5f5f5"
    , style "letter-spacing" "0.4rem"
    , style "text-align" "center"
    ]


lapsStyles : List (Attribute msg)
lapsStyles =
    [ style "padding" "0"
    , style "text-align" "center"
    , style "background" offWhite
    ]


lapTimerStyles : List (Attribute msg)
lapTimerStyles =
    [ style "font-size" "1.2rem"
    , style "text-align" "right"
    , style "color" midGrey
    ]


lapEntryStyles : List (Attribute msg)
lapEntryStyles =
    [ style "list-style" "none"
    , style "padding" "0.5rem 0"
    , style "border-bottom" <| "1px solid" ++ lightGrey
    , style "font-size" "1.2rem"
    , style "display" "flex"
    , style "flex-direction" "row"
    ]


lapNumberStyles : List (Attribute msg)
lapNumberStyles =
    [ style "width" "2em"
    , style "color" midGrey
    ]


lapTimeStyles : List (Attribute msg)
lapTimeStyles =
    [ style "flex" "1"
    , style "padding-right" "2em"
    ]


timersStyles : List (Attribute msg)
timersStyles =
    [ style "padding" "1rem 0"
    , style "display" "inline-block"
    ]


totalTimerStyles : List (Attribute msg)
totalTimerStyles =
    [ style "font-size" "2rem"
    ]


timersWrapperStyles : List (Attribute msg)
timersWrapperStyles =
    [ style "text-align" "center"
    ]
