module Styles exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (..)


type alias Colors =
  { offBlack : String
  , offWhite : String
  , green : String
  , red : String
  , lightGrey : String
  , midGrey : String
  }


colors : Colors
colors =
  { offBlack = "#333"
  , offWhite = "#F5F5F5"
  , green = "#5CEC3D"
  , red = "#EC483D"
  , lightGrey = "#CCC"
  , midGrey = "#777"
  }


buttonBackground : String -> String
buttonBackground actionName =
  case actionName of
    "Start" ->
      colors.green

    "Stop" ->
      colors.red

    "Lap" ->
      colors.lightGrey

    "Reset" ->
      colors.lightGrey

    _ ->
      ""


actionButton : String -> Attribute a
actionButton actionName =
  style
    [ ( "flex-grow", "1" )
    , ( "min-width", "50%" )
    , ( "font-size", "1.2rem" )
    , ( "border", "none" )
    , ( "outline", "none" )
    , ( "padding", "0.4rem" )
    , ( "background", (buttonBackground actionName) )
    ]


buttonRow : Attribute a
buttonRow =
  style
    [ ( "display", "flex" )
    , ( "flex-direction", "row" )
    ]


header : Attribute a
header =
  style
    [ ( "text-transform", "uppercase" )
    , ( "background", colors.offBlack )
    , ( "padding", "0.5em 1rem" )
    , ( "font-size", "1.2rem" )
    , ( "color", "#f5f5f5" )
    , ( "letter-spacing", "0.4rem" )
    , ( "text-align", "center" )
    ]


laps : Attribute a
laps =
  style
    [ ( "padding", "0" )
    , ( "text-align", "center" )
    , ( "background", colors.offWhite )
    ]


lapTimer : Attribute a
lapTimer =
  style
    [ ( "font-size", "1.2rem" )
    , ( "text-align", "right" )
    , ( "color", colors.midGrey )
    ]


lapEntry : Attribute a
lapEntry =
  style
    [ ( "list-style", "none" )
    , ( "padding", "0.5rem 0" )
    , ( "border-bottom", "1px solid" ++ colors.lightGrey )
    , ( "font-size", "1.2rem" )
    , ( "display", "flex" )
    , ( "flex-direction", "row" )
    ]


lapNumber : Attribute a
lapNumber =
  style
    [ ( "width", "2em" )
    , ( "color", colors.midGrey )
    ]


lapTime : Attribute a
lapTime =
  style
    [ ( "flex", "1" )
    , ( "padding-right", "2em" )
    ]


textBlock : Attribute a
textBlock =
  style
    [ ( "margin", "1rem" )
    , ( "text-align", "center" )
    ]


timers : Attribute a
timers =
  style
    [ ( "padding", "1rem 0" )
    , ( "display", "inline-block" )
    ]


totalTimer : Attribute a
totalTimer =
  style
    [ ( "font-size", "2rem" )
    ]


timersWrapper : Attribute a
timersWrapper =
  style [ ( "text-align", "center" ) ]
