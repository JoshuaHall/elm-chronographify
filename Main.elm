module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time)
import Signal exposing (Signal)


type Action
  = Start
  | NoOp


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


ticks : Signal ()
ticks =
  (Time.millisecond * 1000)
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


buttonRow : Html
buttonRow =
  div
    []
    [ button
        [ onClick actions.address Start ]
        [ text "Start" ]
    ]


view : String -> List Int -> Int -> Html
view prefix laps num2 =
  div
    []
    [ viewLine prefix
    , viewLine ("Current: " ++ (num2 |> toString))
    , buttonRow
    , viewLine "Laps:"
    , ul
        []
        (List.map toListItem laps)
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

        NoOp ->
          state

    TimeChange ->
      let
        ( current, laps ) =
          if state.isTiming then
            ( state.current + 1, state.current :: state.laps )
          else
            ( state.current, state.laps )
      in
        { state
          | laps = laps
          , current = current
        }


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
  view "Chronographify" model.laps model.current


main : Signal Html
main =
  Signal.map html model
