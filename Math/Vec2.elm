module Math.Vec2 (..) where

{-|

# Vec2 type
@docs Vec2, x, y, zero

# Operations
@docs bimap, mapBoth, add, sub, mul, div, neg, scale

# Measure
@docs distance, dot, magnitude, direction, angle, lerp

# Misc
@docs fromTuple, normalize
-}


import Focus
import Math.Interpolate as I


{-| a 2d Vector -}
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


{-| An empty Vector -}
zero : Vec2
zero = Vec2 0 0


{-|
Transform both x and y with the same function
-}
mapBoth : (Float -> Float) -> Vec2 -> Vec2
mapBoth f {x,y} =
    {x = f x, y = f y}


{-|
Apply a function accross `x`'s and `y`'s
-}
bimap : (Float -> Float -> Float) -> Vec2 -> Vec2 -> Vec2
bimap f a b =
  { x = f a.x b.x, y = f a.y b.y }


{-|
Vector addition
-}
add : Vec2 -> Vec2 -> Vec2
add =
  bimap (+)


{-|
Vector subtraction
-}
sub : Vec2 -> Vec2 -> Vec2
sub =
  bimap (-)


{-|
Vector multiplication
-}
mul : Vec2 -> Vec2 -> Vec2
mul =
  bimap (*)


{-|
Vector division
-}
div : Vec2 -> Vec2 -> Vec2
div =
  bimap (/)


{-|
Negate a Vector (flip x and y to negative)
-}
neg : Vec2 -> Vec2
neg =
  mapBoth (\z -> -z)


{-|
Scalar Vector multiplication
-}
scale : Float -> Vec2 -> Vec2
scale =
    mapBoth << (*)


{-|
Dot Product
-}
dot : Vec2 -> Vec2 -> Float
dot a b =
  a.x * b.x + a.y * b.y


{-|
Get magnitude of a Vector (length of the Vector)
-}
magnitude : Vec2 -> Float
magnitude v =
  dot v v
    |> sqrt


{-|-}
angle : Vec2 -> Float
angle v =
  atan2 v.y v.x


{-|
Get the distance between two Vectors
-}
distance : Vec2 -> Vec2 -> Float
distance a b =
  let
    { x, y } =
      sub a b
  in
    sqrt <| x * x + y * y


{-|
A unit vector with the same direction as the given vector: a / |a|
-}
normalize : Vec2 -> Vec2
normalize v =
  scale (1.0 / magnitude v) v


{-|
The normalized direction from b to a: (a - b) / |a - b|
-}
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


{-|
Find the Vec between 2 Vecs at a given position
for example:
```
x(0,2)
    z(1,3.5)
          y(2,5)

z = lerp 0.5 x y
```
where 0.5 is the placement of z between x and y
 -}
lerp : Float -> Vec2 -> Vec2 -> Vec2
lerp =
    bimap << I.lerp
