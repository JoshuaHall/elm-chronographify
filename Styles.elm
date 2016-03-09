module Styles (..) where

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


actionButton : String -> Attribute
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


buttonRow : Attribute
buttonRow =
  style
    [ ( "display", "flex" )
    , ( "flex-direction", "row" )
    ]


header : Attribute
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


laps : Attribute
laps =
  style
    [ ( "padding", "0" )
    , ( "text-align", "center" )
    , ( "background", colors.offWhite )
    ]


lapTimer : Attribute
lapTimer =
  style
    [ ( "font-size", "1.2rem" )
    , ( "text-align", "right" )
    , ( "color", colors.midGrey )
    ]


lapEntry : Attribute
lapEntry =
  style
    [ ( "list-style", "none" )
    , ( "padding", "0.5rem 0" )
    , ( "border-bottom", "1px solid" ++ colors.lightGrey )
    , ( "font-size", "1.2rem" )
    ]


textBlock : Attribute
textBlock =
  style
    [ ( "margin", "1rem" )
    , ( "text-align", "center" )
    ]


timers : Attribute
timers =
  style
    [ ( "padding", "1rem 0" )
    , ( "display", "inline-block" )
    ]


totalTimer : Attribute
totalTimer =
  style
    [ ( "font-size", "2rem" )
    ]


timersWrapper : Attribute
timersWrapper =
  style [ ( "text-align", "center" ) ]
