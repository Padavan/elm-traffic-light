module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Svg exposing ( svg, circle, Svg )
import Time
import Task
--import Svg.Attributes exposing (..)

main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

-- MODEL

type alias Model =
  { green : Bool
  , red: Bool
  , yellow: Bool
  , mode: Int
  }


init : () -> (Model, Cmd Msg)
init _ =
  (
    {red = True
    , yellow = True
    , green = False
    , mode = 0
    },
    Task.perform Tick Time.now
  )

-- UPDATE
type Msg = Tick Time.Posix | Off | UnContolled

update : Msg -> Model -> (Model, Cmd Msg)

update msg model =
  case msg of
    Off ->
      ({ red = False
      , yellow = False
      , green = False
      , mode = 0
      }, Cmd.none)
    UnContolled ->
      ({ red = False
      , yellow = False
      , green = False
      , mode = 1
      }, Cmd.none)
    Tick newTime -> 
      if model.mode == 1 then
        ({ red = False
        , yellow =
          if model.yellow == True then False else True
        , green = False
        , mode = 1
        }, Cmd.none)
      else (model, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick



-- VIEW

view : Model -> Html Msg
view model = 
  div []
    [ div 
      [ style "border" "1px solid black"
      ]
      [ trafficLightCircle "red" model.red "50"
      , trafficLightCircle "yellow" model.yellow "125"
      , trafficLightCircle "green" model.green "200"
      ]
    , div [ style "display" "flex"]
      [ button [onClick Off] [ text "Off"]
      , button [onClick UnContolled] [ text "Not Regulating"]
      ]

    ]

trafficLightCircle: String -> Bool -> String -> Svg msg
trafficLightCircle color state position =
  let
    currentColor: String
    currentColor =
      if state == False then "grey" else color
  in
  div
  [ style "border" "1px solid black"
  , style "border-radius" "50%"
  , style "width" "50px"
  , style "height" "50px"
  , style "background-color" currentColor
  ][]
    
--<circle cx="50" cy="50" r="40" stroke="black" stroke-width="3" fill="red" />