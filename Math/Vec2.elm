module Math.Vec2 (..) where

{-|

# Vec2 type
@docs Vec2, x, y

# Operations
@docs bimap, mapBoth, add, sub, mul, div, neg, scale

# Measure
@docs distance, dot, magnitude, direction, angle

# Misc
@docs fromTuple, normalize
-}

import Focus

{-|-}
type alias Vec2 =
  { x : Float, y : Float }

{-|-}
x : Focus.Focus { b | x : a } a
x =
  Focus.create .x (\f big -> { big | x = f big.x })

{-|-}
y : Focus.Focus { b | y : a } a
y =
  Focus.create .y (\f big -> { big | y = f big.y })

{-|
transform both x and y with the same function
-}
mapBoth : (Float -> Float) -> Vec2 -> Vec2
mapBoth f {x,y} =
    {x = f x, y = f y}


{-|
apply a function accross `x`'s and `y`'s
-}
bimap : (Float -> Float -> Float) -> Vec2 -> Vec2 -> Vec2
bimap f a b =
  { x = f a.x b.x, y = f a.y b.y }

{-|-}
add : Vec2 -> Vec2 -> Vec2
add =
  bimap (+)

{-|-}
sub : Vec2 -> Vec2 -> Vec2
sub =
  bimap (-)

{-|-}
mul : Vec2 -> Vec2 -> Vec2
mul =
  bimap (*)

{-|-}
div : Vec2 -> Vec2 -> Vec2
div =
  bimap (/)

{-|-}
neg : Vec2 -> Vec2
neg =
  mapBoth (\z -> -z)

{-|-}
scale : Float -> Vec2 -> Vec2
scale s v =
  { x = v.x * s, y = v.y * s }

{-|-}
dot : Vec2 -> Vec2 -> Float
dot a b =
  a.x * b.x + a.y * b.y

{-|-}
magnitude : Vec2 -> Float
magnitude v =
  dot v v
    |> sqrt

{-|-}
angle : Vec2 -> Float
angle v =
  atan2 v.y v.x

{-|-}
distance : Vec2 -> Vec2 -> Float
distance a b =
  let
    { x, y } =
      sub a b
  in
    sqrt <| x * x + y * y

{-|-}
normalize : Vec2 -> Vec2
normalize v =
  scale (1.0 / magnitude v) v

{-|-}
direction : Vec2 -> Vec2 -> Vec2
direction a b =
  let
    r =
      { x = a.x - b.x, y = a.y - b.y }

    im =
      1.0 / magnitude r
  in
    { x = r.x * im
    , y = r.y * im
    }

{-| Convert (x,y) tuple into a Vec2 -}
fromTuple : ( Float, Float ) -> Vec2
fromTuple =
  uncurry Vec2
