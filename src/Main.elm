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
  , timer: Int
  }

init : () -> (Model, Cmd Msg)
init _ =
  (
    {red = True
    , yellow = True
    , green = True
    , mode = 5
    , timer = 0
    },
    Task.perform Tick Time.now
  )

-- UPDATE

type Msg = Tick Time.Posix | ChangeMode Int | SetLight String

update : Msg -> Model -> (Model, Cmd Msg)

update msg model =
  case msg of
    SetLight light ->
      case light of
        "green" ->
          ({ red = False, yellow = False, green = True, mode = model.mode, timer = 0 }, Cmd.none)
        "to red" -> ({ red = False, yellow = True, green = False, mode = model.mode, timer = 0 }, Cmd.none)
        "to green" -> ({ model | red = True, yellow = True, green = False }, Cmd.none)
        "red" -> ({ red = True, yellow = False, green = False, mode = model.mode, timer = 0 }, Cmd.none)
        _ -> ({ red = model.red, yellow = model.yellow, green = model.green, mode = model.mode, timer = 0 }, Cmd.none)
    Tick newTime -> 
      if model.mode == 1 then
        ({ red = False
        , yellow =
          if model.yellow == True then False else True
        , green = False
        , mode = 1
        , timer = 0
        }, Cmd.none)
      else if model.mode == 3 then
        if (model.timer == 3 && model.green == True) then ({ model | green = False, yellow = True, timer = 0}, Cmd.none)
        else if (model.timer == 3 && model.red == True) then ({ model | yellow = True, timer = 0}, Cmd.none)
        else if (model.timer == 2 && model.red == True && model.yellow == True) then ({ model | yellow = False, green = True, red = False, timer = 0}, Cmd.none)
        else if (model.timer == 2 && model.red == False && model.yellow == True) then ({ model | yellow = False, red = True, timer = 0}, Cmd.none)
        else ({ model | timer = model.timer + 1 }, Cmd.none)
      else (model, Cmd.none)
    ChangeMode modeNumber ->
      --Debug.log "numberlog" modeNumber
      ({ red = if modeNumber == 3 then True else False
      , yellow = False
      , green = False
      , mode = modeNumber
      , timer = 0}, Cmd.none)


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
      [ button [onClick (ChangeMode 0)] [ text "Off"]
      , button [onClick (ChangeMode 1)] [ text "Not Regulating"]
      , button [onClick (ChangeMode 2) ] [ text "Manual"]
      , button [onClick (ChangeMode 3) ] [ text "Auto"]
      ]
    , div [ style "display" "flex"]
      [manualButtons model]
      
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
    
manualButtons: Model -> Html Msg
manualButtons model = 
  if model.mode == 2
    then
      div [] 
        [ button [onClick (SetLight "green"), disabled (model.green == True)][ text "green"]
        , button [onClick (SetLight "to green"), disabled (model.yellow == True)][ text "to green"]
        , button [onClick (SetLight "to red"), disabled (model.yellow == True)][ text "to red"]
        , button [onClick (SetLight "red"), disabled (model.red == True)][ text "red"]
        ]
    else
      text ""
  