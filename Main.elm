module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time)
import Signal exposing (Signal)
import String


type Action
  = Start
  | Stop
  | Lap
  | Reset
  | NoOp


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


millis : Signal Time
millis =
  Time.every Time.millisecond


viewLine : String -> Html
viewLine lineText =
  div
    [ style
        [ ( "margin", "1em" )
        , ( "width", "20rem" )
        ]
    ]
    [ text lineText ]


millisToString : Time -> String
millisToString millis =
  let
    minutes =
      floor (millis / (60 * 1000))

    seconds =
      floor (millis / 1000)

    centiseconds =
      (floor millis `rem` 1000) // 10

    pad n =
      String.padLeft 2 '0' (toString n)
  in
    (pad minutes) ++ ":" ++ (pad seconds) ++ "," ++ (pad centiseconds)


buttonRow : Bool -> Html
buttonRow isTiming =
  div
    []
    <| if isTiming then
        [ actionButton Stop
        , actionButton Lap
        ]
       else
        [ actionButton Start
        , actionButton Reset
        ]


actionButton : Action -> Html
actionButton action =
  button
    [ onClick actions.address action ]
    [ text (toString action) ]


view : String -> Model -> Html
view heading model =
  let
    timeDiff =
      model.currentTimestamp - model.startTimestamp

    currentDuration =
      if model.isTiming then
        model.durationOnStop + timeDiff
      else
        model.durationOnStop
  in
    div
      []
      [ viewLine heading
      , viewLine (millisToString currentDuration)
      , buttonRow model.isTiming
      , viewLine "Laps:"
      , ul
          []
          (List.map toListItem model.laps)
      ]


toListItem : Time -> Html
toListItem num =
  li [] [ text (millisToString num) ]


type alias Model =
  { startTimestamp : Time
  , laps : List Time
  , isTiming : Bool
  , currentTimestamp : Time
  , durationOnStop : Time
  , lapDurationOnStop : Time
  , lastLapTimestamp : Time
  }


initial : Model
initial =
  { startTimestamp = 0
  , laps = []
  , isTiming = False
  , currentTimestamp = 0
  , durationOnStop = 0
  , lapDurationOnStop = 0
  , lastLapTimestamp = 0
  }


update : StateChange -> Model -> Model
update change state =
  case change of
    ButtonPress ( time, action ) ->
      case action of
        Start ->
          { state
            | isTiming = True
            , startTimestamp = time
            , lastLapTimestamp = time
          }

        Stop ->
          let
            duration =
              state.durationOnStop + time - state.startTimestamp
          in
            { state
              | isTiming = False
              , durationOnStop = duration
              , lapDurationOnStop = duration - (total state.laps)
            }

        Lap ->
          { state
            | laps = (state.lapDurationOnStop + time - state.lastLapTimestamp) :: state.laps
            , lastLapTimestamp = time
            , lapDurationOnStop = 0
          }

        Reset ->
          initial

        NoOp ->
          state

    TimeChange time ->
      if state.isTiming then
        { state | currentTimestamp = time }
      else
        state


total : List Time -> Time
total laps =
  List.foldl (+) 0 laps


type StateChange
  = TimeChange Time
  | ButtonPress ( Time, Action )


inputSignal : Signal StateChange
inputSignal =
  Signal.mergeMany
    [ Signal.map TimeChange millis
    , Signal.map ButtonPress (Time.timestamp actions.signal)
    ]


model : Signal Model
model =
  Signal.foldp update initial inputSignal


html : Model -> Html
html model =
  view "Chronographify" model


main : Signal Html
main =
  Signal.map html model
