module Vec where

type alias Vec = {x : Float, y : Float}

zeroV : Vec
zeroV = {x = 0, y = 0}

unitV : Vec
unitV = normalize {x = 1, y = 1}

iV : Vec
iV = {x = 1, y = 0}
jV : Vec
jV = {x = 0, y = 1}


normalize : Vec -> Vec
normalize v =
  let m = sqrt (v.x^2 + v.y^2)
  in if m == 0 then {x = 0, y = 0} else {x = v.x/m, y = v.y/m}

alter : (Float -> Float) -> Vec -> Vec
alter f v = {x = f v.x, y = f v.y}

magnitude : Vec -> Float
magnitude v = sqrt (v.x*v.x + v.y+v.y)

dot : Vec -> Vec -> Float
dot u v = u.x*v.x + u.y*v.y

angle : Vec -> Vec -> Float
angle u v = acos (dot u v / (magnitude u * magnitude v))

scale : Float -> Vec -> Vec
scale f v = {x = v.x*f, y = v.y*f}

add : Vec -> Vec -> Vec
add u v = {x = u.x + v.x, y = u.y + v.y}

sub : Vec -> Vec -> Vec
sub u v = {x = u.x-v.x, y = u.y-v.y}

rotateV : Float -> Vec -> Vec
rotateV angle v =
  let
    sn = sin angle
    cs = cos angle
  in
    {x = cs*v.x-sn*v.y, y = sn*v.x+cs*v.y}


