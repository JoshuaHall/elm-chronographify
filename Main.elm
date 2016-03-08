module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time)
import Signal exposing (Signal)


seconds : Signal ()
seconds =
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


view : String -> List Int -> Int -> Html
view prefix laps num2 =
  div
    []
    [ viewLine prefix
    , viewLine ("Current: " ++ (num2 |> toString))
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


update : () -> Model -> Model
update _ state =
  let
    laps =
      state.current :: state.laps

    current =
      state.current + 1
  in
    { state
      | laps = laps
      , current = current
    }


model : Signal Model
model =
  Signal.foldp update initial seconds


html : Model -> Html
html model =
  view "Increditimer" model.laps model.current


main : Signal Html
main =
  Signal.map html model
