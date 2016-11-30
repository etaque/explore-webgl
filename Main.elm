module Main exposing (..)

import Math.Vector2 exposing (..)
import WebGL exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (width, height)
import AnimationFrame
import Time exposing (Time)
import Hexagons


-- Create a mesh with two triangles


type alias Vertex =
    { position : Vec2 }


mesh : Drawable Vertex
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
            [ ( Vertex (vec2 -w2 -h4), Vertex (vec2 0 -h2), Vertex (vec2 0 0) )
            , ( Vertex (vec2 0 -h2), Vertex (vec2 w2 -h4), Vertex (vec2 0 0) )
            , ( Vertex (vec2 w2 -h4), Vertex (vec2 w2 h4), Vertex (vec2 0 0) )
            , ( Vertex (vec2 w2 h4), Vertex (vec2 0 h2), Vertex (vec2 0 0) )
            , ( Vertex (vec2 0 h2), Vertex (vec2 -w2 h4), Vertex (vec2 0 0) )
            , ( Vertex (vec2 -w2 h4), Vertex (vec2 -w2 -h4), Vertex (vec2 0 0) )
            ]


main : Program Never Time Time
main =
    Html.program
        { init = ( 0, Cmd.none )
        , view = view
        , subscriptions =
            \model -> Sub.none
            -- (\model -> AnimationFrame.diffs Basics.identity)
        , update = (\elapsed currentTime -> ( elapsed + currentTime, Cmd.none ))
        }


view : Float -> Html msg
view t =
    WebGL.toHtml
        [ width 400, height 400 ]
        [ render vertexShader fragmentShader mesh {} ]



-- Shaders


vertexShader : Shader { attr | position : Vec2 } {} {}
vertexShader =
    [glsl|

attribute vec2 position;

void main () {
    gl_Position = vec4(position, 0, 1);
}

|]


fragmentShader : Shader {} u {}
fragmentShader =
    [glsl|

precision mediump float;

void main () {
    gl_FragColor = vec4(0, 1, 0, 1);
}

|]
