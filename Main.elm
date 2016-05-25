module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Time exposing (Time)
import String
import Task
import Styles


main : Program Never
main =
  Html.program
    { init = init
    , update = update
    , view = view "Chronographify"
    , subscriptions = millis
    }



-- Subscriptions


millis : Model -> Sub Msg
millis model =
  Time.every (Time.millisecond * 16) TimeChange



-- Model


type alias Model =
  { startTimestamp : Time
  , laps : List Time
  , isTiming : Bool
  , currentTimestamp : Time
  , durationOnStop : Time
  , lapDurationOnStop : Time
  , lastLapTimestamp : Time
  }


init : ( Model, Cmd a )
init =
  ( { startTimestamp = 0
    , laps = []
    , isTiming = False
    , currentTimestamp = 0
    , durationOnStop = 0
    , lapDurationOnStop = 0
    , lastLapTimestamp = 0
    }
  , Cmd.none
  )



-- Update


type Msg
  = ReceiveTimestamp UserAction Time
  | TimeChange Time
  | ButtonPress UserAction
  | NoOp


type UserAction
  = Start
  | Stop
  | Lap
  | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update change state =
  case change of
    ButtonPress userAction ->
      ( state, Task.perform (\_ -> NoOp) (ReceiveTimestamp userAction) Time.now )

    ReceiveTimestamp userAction time ->
      case userAction of
        Start ->
          ( handleStart time state, Cmd.none )

        Stop ->
          ( handleStop time state, Cmd.none )

        Lap ->
          ( handleLap time state, Cmd.none )

        Reset ->
          init

    TimeChange time ->
      if state.isTiming then
        ( { state | currentTimestamp = time }, Cmd.none )
      else
        ( state, Cmd.none )

    NoOp ->
      ( state, Cmd.none )


handleStart : Time -> Model -> Model
handleStart time state =
  { state
    | isTiming = True
    , startTimestamp = time
    , lastLapTimestamp = time
  }


handleStop : Time -> Model -> Model
handleStop time state =
  let
    duration =
      state.durationOnStop + time - state.startTimestamp
  in
    { state
      | isTiming = False
      , durationOnStop = duration
      , lapDurationOnStop = duration - (total state.laps)
    }


handleLap : Time -> Model -> Model
handleLap time state =
  { state
    | laps = (state.lapDurationOnStop + time - state.lastLapTimestamp) :: state.laps
    , lastLapTimestamp = time
    , lapDurationOnStop = 0
  }


total : List Time -> Time
total laps =
  List.foldl (+) 0 laps


millisToString : Time -> String
millisToString millis =
  let
    minutes =
      floor (millis / (60 * 1000))

    seconds =
      floor (millis / 1000) `rem` 60

    centiseconds =
      (floor millis `rem` 1000) // 10

    pad n =
      String.padLeft 2 '0' (toString n)
  in
    (pad minutes) ++ ":" ++ (pad seconds) ++ "," ++ (pad centiseconds)



-- View


view : String -> Model -> Html Msg
view heading model =
  div []
    [ header heading
    , timers model
    , buttonRow model.isTiming
    , lapsList model.laps
    ]


header : String -> Html Msg
header heading =
  div [ Styles.header ]
    [ text heading ]


timers : Model -> Html Msg
timers model =
  let
    timeDiff =
      model.currentTimestamp - model.startTimestamp

    currentDuration =
      if model.isTiming then
        model.durationOnStop + timeDiff
      else
        model.durationOnStop

    lapDuration =
      currentDuration - total model.laps
  in
    div [ Styles.timersWrapper ]
      [ div [ Styles.timers ]
          [ div [ Styles.lapTimer ] [ text (millisToString lapDuration) ]
          , div [ Styles.totalTimer ] [ text (millisToString currentDuration) ]
          ]
      ]


buttonRow : Bool -> Html Msg
buttonRow isTiming =
  div [ Styles.buttonRow ]
    <| if isTiming then
        [ actionButton Stop
        , actionButton Lap
        ]
       else
        [ actionButton Start
        , actionButton Reset
        ]


lapsList : List Time -> Html Msg
lapsList laps =
  ul [ Styles.laps ]
    (laps
      |> List.reverse
      |> List.indexedMap lapEntry
      |> List.reverse
    )


actionButton : UserAction -> Html Msg
actionButton action =
  button
    [ onClick (ButtonPress action)
    , (Styles.actionButton (toString action))
    ]
    [ text (toString action) ]


lapEntry : Int -> Time -> Html Msg
lapEntry index entry =
  let
    lapNumber =
      (toString (index + 1)) ++ "."
  in
    li [ Styles.lapEntry ]
      [ span [ Styles.lapNumber ] [ text lapNumber ]
      , span [ Styles.lapTime ] [ text (millisToString entry) ]
      ]


viewLine : String -> Html Msg
viewLine lineText =
  div [ Styles.textBlock ]
    [ text lineText ]
