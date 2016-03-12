import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window
import Color exposing (rgb)

type alias Station = { x : Float, y : Float, vel : Float, mass : Float , radius : Float}

type alias Planet = { x : Float, y : Float, vel : Float, mass : Float, radius : Float }

type alias Vec = { x : Float, y : Float }

type alias Action = { vel : Float }

type State = Play | Pause

type alias Game =
  { state : State
  , station : Station
  , planets : List Planet }

defaultGame : Game
defaultGame =
  { state = Pause
  , station = {x = 200, y = 200, vel = 0, mass = 1, radius = 50}
  , planets = [{x = 0, y = 0, vel = 0, mass = 100, radius = 200}]}

update : Action -> Game -> Game
update action game = {game | station = accel action.vel game.station }

accel : Float -> Station -> Station
accel v s = { s | vel = s.vel + v }

normalize : Vec -> Vec
normalize v =
  let m = (v.x^2 + v.y^2)
  in {x = v.x/m, y = v.y/m}

scale : Float -> Vec -> Vec
scale f v = {x = v.x*f, y = v.y*f}

gravity : Station -> Planet -> Vec
gravity s p =
  let
    d2 = sqrt ((s.x - p.x)^2 + (s.y - p.y)^2)
    k = 6.674*10^(-11) --N(m/kg)^2
    f = k * s.mass * p.mass / d2
    dir = normalize {x = s.x-p.x, y = s.y-p.y}
  in
    scale f dir

view : (Int, Int) -> Game -> Element
view (w, h) g = container w h middle  <|
                collage w h
                        ([ rect (toFloat w) (toFloat h) |> filled spaceC
                        , oval g.station.radius g.station.radius |> filled whiteC |> move (g.station.x, g.station.y)
                        ] ++ List.map (\p -> oval p.radius p.radius |> filled planetC |> move (p.x, p.y)) g.planets)

spaceC = rgb 60 100 60

whiteC = rgb 100 100 100

planetC = rgb 80 80 20

gameState : Signal Game
gameState = Signal.foldp update defaultGame input

delta = Signal.map inSeconds (fps 30)

input : Signal Action
input = Signal.sampleOn delta <| Signal.map (Action << toFloat)  (Signal.map .y Keyboard.arrows)


main = Signal.map2 view Window.dimensions gameState
