module Main exposing (..)

import Math.Vector2 as Vec2 exposing (..)
import WebGL exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (width, height)
import AnimationFrame
import Time exposing (Time)
import Hexagons
import Hexagons.Grid as Grid exposing (Grid)
import Dict
import Window
import Task


type Msg
    = WindowSize Window.Size
    | Elapsed Time


type alias Model =
    { time : Time
    , size : Window.Size
    , grid : Grid WindCell
    }


initialModel : Model
initialModel =
    Model 0 { width = 100, height = 100 } grid


type alias WindCell =
    { speed : Float }


grid : Grid.Grid WindCell
grid =
    Dict.empty
        |> Grid.set (WindCell 2) ( -1, 0 )
        |> Grid.set (WindCell 4) ( 0, 0 )
        |> Grid.set (WindCell 6) ( 1, 0 )
        |> Grid.set (WindCell 8) ( 2, 0 )
        |> Grid.set (WindCell 1) ( 0, 1 )
        |> Grid.set (WindCell 3) ( 1, 1 )


mesh : Drawable Attribute
mesh =
    let
        ( w, h ) =
            Hexagons.dims 1

        w2 =
            w / 2

        h2 =
            h / 2

        h4 =
            h / 4
    in
        Triangle
            [ ( Attribute (vec2 -w2 -h4), Attribute (vec2 0 -h2), Attribute (vec2 0 0) )
            , ( Attribute (vec2 0 -h2), Attribute (vec2 w2 -h4), Attribute (vec2 0 0) )
            , ( Attribute (vec2 w2 -h4), Attribute (vec2 w2 h4), Attribute (vec2 0 0) )
            , ( Attribute (vec2 w2 h4), Attribute (vec2 0 h2), Attribute (vec2 0 0) )
            , ( Attribute (vec2 0 h2), Attribute (vec2 -w2 h4), Attribute (vec2 0 0) )
            , ( Attribute (vec2 -w2 h4), Attribute (vec2 -w2 -h4), Attribute (vec2 0 0) )
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Task.perform WindowSize Window.size )
        , view = view
        , subscriptions =
            \model -> Window.resizes WindowSize
            -- (\model -> AnimationFrame.diffs Basics.identity)
        , update = update
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowSize size ->
            { model | size = size } ! []

        Elapsed delta ->
            { model | time = model.time + delta } ! []


hexRadius : Float
hexRadius =
    50


view : Model -> Html msg
view model =
    WebGL.toHtml
        [ width model.size.width, height model.size.height ]
        (List.map (renderCell model.size) (Grid.list model.grid))


renderCell : Window.Size -> Grid.Tile WindCell -> Renderable
renderCell { width, height } tile =
    let
        uniform =
            { speed = tile.content.speed
            , offset = Vec2.fromTuple (Hexagons.axialToPoint hexRadius tile.coords)
            , windowSize = vec2 (toFloat width) (toFloat height)
            , hexSize = Vec2.fromTuple (Hexagons.dims hexRadius)
            }
    in
        render vertexShader fragmentShader mesh (Debug.log "uniform" uniform)


type alias Attribute =
    { position : Vec2 }


type alias Uniform u =
    { u
        | speed : Float
        , offset : Vec2
        , windowSize : Vec2
        , hexSize : Vec2
    }


type alias Varying =
    { vSpeed : Float }


vertexShader : Shader Attribute (Uniform u) Varying
vertexShader =
    [glsl|
  attribute vec2 position;
  uniform float speed;
  uniform vec2 offset;
  uniform vec2 hexSize;
  uniform vec2 windowSize;
  varying float vSpeed;

  void main () {
      vec2 clipSpace = position * vec2(hexSize.y, hexSize.y) / windowSize;
      vec2 offsetClipSpace = offset / windowSize * 2.0;
      gl_Position = vec4(vec2(clipSpace.x + offsetClipSpace.x, clipSpace.y - offsetClipSpace.y), 0, 1);
      vSpeed = speed;
  }
|]


fragmentShader : Shader {} u Varying
fragmentShader =
    [glsl|
  precision mediump float;
  varying float vSpeed;

  void main () {
      gl_FragColor = vec4(0, 1.0 - vSpeed * 0.1, 0, 1);
  }
|]
