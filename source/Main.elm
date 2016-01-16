module Main where

import Signal exposing (forwardTo)
import StartApp
import Effects
import Task

import Html exposing (div, span, strong, text, node)

import Ui.Container
import Ui.Button
import Ui.App
import Ui

import Docs.Chooser as Chooser
import Docs.Button as Button

type alias Model =
  { app : Ui.App.Model
  , button : Button.Model
  , chooser : Chooser.Model
  , active : Active
  }

type Active
  = Button
  | Chooser

type Action
  = App Ui.App.Action
  | ButtonAction Button.Action
  | ChooserAction Chooser.Action
  | Switch

init : Model
init =
  { app = Ui.App.init "Elm-UI Project"
  , button = Button.init
  , chooser = Chooser.init
  , active = Button
  }

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    App act ->
      let
        (app, effect) = Ui.App.update act model.app
      in
        ({ model | app = app }, Effects.map App effect)

    Switch ->
      if model.active == Button then
        ({ model | active = Chooser }, Effects.none)
      else
        ({ model | active = Button }, Effects.none)

    ButtonAction act ->
      let
        (button, effect) = Button.update act model.button
      in
        ({ model | button = button }, Effects.map ButtonAction effect)

    ChooserAction act ->
      let
        (chooser, effect) = Chooser.update act model.chooser
      in
        ({ model | chooser = chooser }, Effects.map ChooserAction effect)

view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    (componentView, componentFields) =
      case model.active of
        Button ->
          Button.render (forwardTo address ButtonAction) model.button
        Chooser ->
          Chooser.render (forwardTo address ChooserAction) model.chooser
  in
    Ui.App.view (forwardTo address App) model.app
      [ Ui.Container.column []
        [ Ui.title [] [text "Elm-UI Playground"]
        , Ui.Button.primary "Switch" address Switch
        , Ui.Container.row []
          [ node "div" [] [componentView]
          , componentFields
          ]
        ]
      ]

app =
  StartApp.start { init = (init, Effects.none)
                 , update = update
                 , view = view
                 , inputs = []
                 }

main =
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
