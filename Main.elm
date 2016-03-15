import Char exposing (..)
import Color exposing (rgb)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input exposing (dropDown, button)
import Keyboard
import Signal
import Text
import Time exposing (..)
import Window

import Vec exposing (..)


rotationSensitivity : Float
rotationSensitivity = 0.1


p0Color : Color.Color
p0Color = rgb 250 0 0

p1Color : Color.Color
p1Color = rgb 0 250 0


type alias Body = {pos : Vec, vel : Vec, acc : Vec, mass : Float, radius : Float}

type alias BoundingBox = {x : Float, y : Float, w : Float, h : Float}

type alias Player = {body : Body, dir : Float, thrust : Bool, life : Int, bullets : Int}
type alias Planet = {body : Body}
type alias Controls = {rotation : Float, thrust : Float, fire : Bool}
type alias Input =
  { reset : Bool
  , pause : Bool
  , player0 : Controls
  , player1 : Controls
  , dt : Float}

type alias Config =
  { rotationSensitivity : Float
  , enginePower : Float
  , bulletRadius : Float
  , bulletMass : Float
  , maxBullets : Int}


type alias Bullet = Body
type alias Game =
  { paused : Bool
  , boundary : BoundingBox
  , player0 : Player
  , player1 : Player
  , planets : List Planet
  , bullets0 : List Bullet
  , bullets1 : List Bullet}

defaultBody : Body
defaultBody = {pos = zeroV, vel = zeroV, acc = zeroV, mass = 1, radius = 1}

defaultPlayer : Player
defaultPlayer =
  { body = {defaultBody | radius = 10}
  , dir = 0
  , thrust = False
  , life = 0
  , bullets = defaultConfig.maxBullets}

shiftPlayer : Float -> Float -> Player -> Player
shiftPlayer x y p = 
  let
    b = p.body
    b' = {b | pos = {x = p.body.pos.x + x, y = p.body.pos.y + y}}
  in
    {p | body = b'}

defaultGame : Game
defaultGame =
  { paused = True
  , boundary = {x = -200, y = -200, w = 400, h = 400}
  , player0 = shiftPlayer 0 100 defaultPlayer
  , player1 = shiftPlayer 0 -100 defaultPlayer
  , planets = [{body = {pos = {x = 200, y = 0}, vel = zeroV, acc = zeroV, mass = 1*10^14, radius = 25}}
              ,{body = {pos = {x = -200, y = 0}, vel = zeroV, acc = zeroV, mass = 5*10^14, radius = 50}}]
  , bullets0 = []
  , bullets1 = []}

defaultControl : Controls
defaultControl =
  { rotation = 0
  , thrust = 0
  , fire = False }

defaultConfig : Config
defaultConfig =
  { rotationSensitivity = 0.1
  , enginePower = 1
  , bulletRadius = 1
  , bulletMass = 1
  , maxBullets = 100}


updatePlayer : Config -> BoundingBox -> List Body -> Float -> Float -> Float -> Bool -> Player -> Player
updatePlayer c boundary bodys dt rot thrust fire p =
  let
    tmp = rot*c.rotationSensitivity + p.dir
    dir' = if tmp > 2*pi then tmp-2*pi else if tmp < negate 2*pi then tmp+2*pi else tmp
    engine = rotateV dir' (Vec.scale (c.enginePower*thrust) {x = 1, y = 0})
    gravs = List.map (\b -> gravity p.body b) bodys
    b' = updateBody dt (engine::gravs) p.body
    nextbdy = bounds boundary p.body (collision bodys p.body b')
    bs' = if fire && p.bullets > 0 then p.bullets - 1 else p.bullets
  in
    {p | body = nextbdy, dir = dir', thrust = thrust /= 0, bullets = bs'}

bounds : BoundingBox -> Body -> Body -> Body
bounds box old nu =
  if nu.pos.x < box.x || nu.pos.x > box.x + box.w
    then {nu | pos = old.pos, vel = {x = negate nu.vel.x, y = nu.vel.y}} --left or right
    else if nu.pos.y < box.y || nu.pos.y > box.y + box.h
           then {nu | pos = old.pos, vel = {x = nu.vel.x, y = negate nu.vel.y}} --above or below
           else nu
  

collision : List Body -> Body -> Body -> Body
collision bodys old nu = 
  case bodys of
    [] -> nu
    (b::bs) ->
      if intersects b nu
        then 
          let 
            n = Vec.normalize (sub b.pos old.pos) --normal
            u = Vec.scale (Vec.dot nu.vel n) n --orthogonal to tangent
            w = Vec.sub nu.vel u --parallel to tangent
            v' = Vec.sub w u --perfectly elastic, frictionless collision
          in 
            {old | vel = v'}
        else collision bs old nu

updateBullet : BoundingBox -> List Body -> Float -> Bullet -> Bullet
updateBullet boundary bodys dt bullet =
  let
    gravs = List.map (gravity bullet) bodys
  in
    bounds boundary bullet (updateBody dt gravs bullet)

intersects : Body -> Body -> Bool
intersects b0 b1 = (b0.pos.x - b1.pos.x)^2 + (b0.pos.y - b1.pos.y)^2 < (b0.radius + b1.radius)^2


calcHits : Player -> List Bullet -> (List Bullet, Player)
calcHits =
  let
    go : List Bullet -> Player -> List Bullet -> (List Bullet, Player)
    go accum p bs =
      case bs of
        [] -> (accum, p)
        (b::bs') ->
          if intersects b p.body
            then go accum {p | life = p.life - round (b.mass)} bs'
            else go (b::accum) p bs'
  in
    go []

fire : Config -> Player -> Bullet
fire c p =
  { pos = p.body.pos
  , vel = (Vec.scale (2*max 10 (Vec.magnitude p.body.vel)) (rotateV p.dir iV))
  , acc = zeroV
  , mass = c.bulletMass
  , radius = c.bulletRadius}

update : (Float, Float, Config, Input) -> Game -> Game
update (w, h, c, inp) game =
  if inp.reset
    then defaultGame
    else
      if xor inp.pause game.paused
        then {game | paused = True} 
        else
           let
             boundary' = {x = negate w/2, y = negate h/2, w = w, h = h}
             pbs = List.map .body game.planets
             p0' = updatePlayer c boundary' pbs inp.dt inp.player0.rotation inp.player0.thrust inp.player0.fire game.player0
             p1' = updatePlayer c boundary' pbs inp.dt inp.player1.rotation inp.player1.thrust inp.player1.fire game.player1
             bullets0' = if inp.player0.fire && game.player0.bullets > 0
                            then fire c game.player0 :: game.bullets0
                            else game.bullets0
             bullets1' = if inp.player1.fire && game.player1.bullets > 0
                            then fire c game.player1 :: game.bullets1
                            else game.bullets1
             (uBs1', up0') = calcHits p0' (List.map (updateBullet boundary' pbs inp.dt) bullets1')
             (uBs0', up1') = calcHits p1' (List.map (updateBullet boundary' pbs inp.dt) bullets0')
           in
            { game | paused = False
            , boundary = boundary'
            , player0 = up0'
            , player1 = up1'
            , bullets0 = uBs0'
            , bullets1 = uBs1'}

updateBody : Time -> List Vec -> Body -> Body
updateBody dt forces b =
  let
    acc = Vec.scale (1/b.mass) (List.foldr add {x = 0, y = 0} forces)
    v' = add acc b.vel
    p' = add (Vec.scale dt v') b.pos
  in
    {b | pos = p', vel = v', acc = acc}

gravity : Body -> Body -> Vec
gravity m0 m1 =
  let
    d2 = (m1.pos.x - m0.pos.x)^2 + (m1.pos.y - m0.pos.y)^2
    k = 6.674*10^(-11) --N(m/kg)^2
    f = k * m1.mass * m0.mass / d2
    dir = normalize {x = m1.pos.x-m0.pos.x, y = m1.pos.y-m0.pos.y}
  in
    Vec.scale f dir

------------------------------ VIEW ------------------------------
viewPlayer : Player -> Color.Color -> Form
viewPlayer s c =
  let
    exhaust = oval (2*s.body.radius) (0.5*s.body.radius) |> filled (rgb 255 255 0) |> move (-3*s.body.radius, 0)
    shape = [ oval (3*s.body.radius) (1.5*s.body.radius) |> outlined {defaultLine | color = whiteC}
            , ngon 3 (1.5*s.body.radius) |> outlined {defaultLine | color = whiteC}
            , ngon 3 (1.5*s.body.radius) |> filled c
            , oval (3*s.body.radius) (1.5*s.body.radius) |> filled c]
    final = if s.thrust
               then exhaust :: shape
               else shape
  in
     group final |> move (s.body.pos.x, s.body.pos.y) |> rotate s.dir

playerInfo : Player -> String
playerInfo p =
  "{px: " ++ toString (round p.body.pos.x) ++ ", py: " ++ toString (round p.body.pos.y) ++ "} " ++
  "{vx: " ++ toString (round p.body.vel.x) ++ ", vy: " ++ toString (round p.body.vel.y) ++ "} " ++
  "{ax: " ++ toString (round p.body.acc.x) ++ ", ay: " ++ toString (round p.body.acc.y) ++ "} " ++
  "bullets: " ++ toString p.bullets

viewBullet : Color.Color -> Bullet -> Form
viewBullet c b =
  circle b.radius
    |> outlined (solid c)
    |> move (b.pos.x, b.pos.y)

viewScore : Player -> Player -> Form
viewScore p0 p1 =
  toForm (Text.fromString ("Player 0: " ++ toString p0.life ++ "\nPlayer 1: " ++ toString p1.life)
    |> Text.color whiteC
    |> Text.monospace
    |> Text.height 20
    |> centered)

viewGame : Int -> Int -> Game -> Element
viewGame w h g = 
  container w h middle  <|
    collage w h
      ([ rect (toFloat w) (toFloat h) |> filled spaceC
       , viewPlayer g.player0 p0Color
       , viewPlayer g.player1 p1Color
       , viewScore g.player0 g.player1 |> move (toFloat w/ -2.2, toFloat h/2.1)
       , toForm (Text.fromString (playerInfo g.player0 ++ "\n" ++ playerInfo g.player1)
           |> Text.color whiteC
           |> Text.monospace
           |> Text.height 12
           |> centered)
           |> move (toFloat w / -2.5, 50 - toFloat h /2)
       , toForm (info
           |> Text.color whiteC
           |> Text.monospace
           |> Text.height 12
           |> centered)
           |> move (toFloat w / -3.4, 100 - toFloat h / 2)
       ] ++ List.map (\p -> circle p.body.radius |> filled planetC |> move (p.body.pos.x, p.body.pos.y)) g.planets ++
       List.map (viewBullet p0Color) g.bullets0 ++
       List.map (viewBullet p1Color) g.bullets1)

info : Text.Text
info = Text.fromString "up[w] to thrust, left/right[a, d] to rotate, ctrl[shift] to fire, p to pause, r to restart"

toTxt = leftAligned << Text.fromString

viewConfig : Element
viewConfig = flow right [ toTxt "Engine Power"
                        , epInp
                        , toTxt "Rotation Sensitivity"
                        , rsInp
                        , toTxt "Bullet Radius"
                        , brInp
                        , toTxt "Max Bullets"
                        , mbInp]

view : (Int, Int) -> Game -> Element --Config -> Element
view (w, h) g = -- c =
  flow down
    [ viewConfig
    , viewGame w h g ] --c]

spaceC : Color.Color
spaceC = rgb 0 0 0 --60 100 60

whiteC : Color.Color
whiteC = rgb 250 250 250

planetC : Color.Color
planetC = rgb 80 80 20

------------------------------ INPUTS ------------------------------
gameSig : Signal Game
gameSig = Signal.foldp update defaultGame (Signal.map3 (\(w, h) c i -> (toFloat w, toFloat h, c, i)) Window.dimensions configSig input)

delta : Signal Float
delta = Signal.map inSeconds (fps 30)

getCtrl : Signal {x : Int, y : Int} -> Signal Bool -> Signal Controls
getCtrl dirsig firesig =
  Signal.map2
    (\dirs fire -> {rotation = toFloat (negate dirs.x), thrust = toFloat (abs dirs.y), fire = fire})
    dirsig
    firesig

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map5
      (\r p dt p0 p1 ->
         { reset = r
         , pause = p
         , player0 = p0
         , player1 = p1
         , dt = dt})
      (Keyboard.isDown (Char.toCode 'R'))
      (Keyboard.isDown (Char.toCode 'P'))
      delta
      (getCtrl Keyboard.arrows Keyboard.ctrl)
      (getCtrl Keyboard.wasd Keyboard.shift)

frange : Float -> Float -> Float -> List Float
frange lo hi step =
  if lo >= hi
     then []
     else lo :: frange (lo + step) hi step

irange : Int -> Int -> Int -> List Int
irange lo hi step =
  if lo >= hi
     then []
     else lo :: irange (lo + step) hi step

epMB : Signal.Mailbox Float
epMB = Signal.mailbox defaultConfig.enginePower

epInp : Element
epInp = 
  dropDown (Signal.message epMB.address)
   (List.map (\x -> (toString (round x), x)) (frange 0 51 1))

rsMB : Signal.Mailbox Float
rsMB = Signal.mailbox defaultConfig.rotationSensitivity

rsInp : Element
rsInp = 
  dropDown (Signal.message rsMB.address)
   (List.map (\x -> (toString x, x)) (frange 0 1.01 0.05))

brMB : Signal.Mailbox Float
brMB = Signal.mailbox defaultConfig.bulletRadius

brInp : Element
brInp = 
  dropDown (Signal.message brMB.address)
   (List.map (\x -> (toString x, x)) (frange 0 50 1))

mbMB : Signal.Mailbox Int
mbMB = Signal.mailbox defaultConfig.maxBullets

mbInp : Element
mbInp = 
  dropDown (Signal.message mbMB.address)
   (List.map (\x -> (toString x, x)) (1 :: irange 5 101 5))

infoMB : Signal.Mailbox Bool
infoMB = Signal.mailbox False

infoBtn : Element
infoBtn = button (Signal.message infoMB.address True) "Info"

configSig : Signal Config
configSig =
  Signal.map4
    (\ep rs br mb ->
      { rotationSensitivity = rs
      , enginePower = ep
      , bulletRadius = br
      , bulletMass = 1
      , maxBullets = mb})
    epMB.signal
    rsMB.signal
    brMB.signal
    mbMB.signal

main : Signal Element
main = Signal.map2 view Window.dimensions gameSig
