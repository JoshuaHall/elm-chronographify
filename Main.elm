module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time)
import Signal exposing (Signal)


type Action
  = Start
  | Stop
  | Lap
  | Reset
  | NoOp


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


ticks : Signal ()
ticks =
  Time.millisecond
    |> Time.every
    |> Signal.map (\_ -> ())


viewLine : String -> Html
viewLine lineText =
  div
    [ style
        [ ( "margin", "1em" )
        , ( "width", "20rem" )
        ]
    ]
    [ text lineText ]


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
  div
    []
    [ viewLine heading
    , viewLine ("Current: " ++ (model.current |> toString))
    , buttonRow model.isTiming
    , viewLine "Laps:"
    , ul
        []
        (List.map toListItem model.laps)
    ]


toListItem : Int -> Html
toListItem num =
  li [] [ text (toString num) ]


type alias Model =
  { current : Int
  , laps : List Int
  , isTiming : Bool
  }


initial : Model
initial =
  { current = 0
  , laps = []
  , isTiming = False
  }


update : StateChange -> Model -> Model
update change state =
  case change of
    ButtonPress action ->
      case action of
        Start ->
          { state | isTiming = True }

        Stop ->
          { state | isTiming = False }

        Lap ->
          { state | laps = state.current :: state.laps }

        Reset ->
          initial

        NoOp ->
          state

    TimeChange ->
      case state.isTiming of
        True ->
          { state | current = state.current + 1 }

        False ->
          state


type StateChange
  = TimeChange
  | ButtonPress Action


inputSignal : Signal StateChange
inputSignal =
  Signal.mergeMany
    [ Signal.map (\_ -> TimeChange) ticks
    , Signal.map ButtonPress actions.signal
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
