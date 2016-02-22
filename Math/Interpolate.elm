module Math.Interpolate (..) where

{-|
@docs lerp
-}

{-|
Interpolate the space between 2 values. For example consider
the int range from 10 to 20. A `lerp 0.5 10 20  == 15` as `15` is half way (`0.5`) between `10` and `20`


-}
lerp : Float -> Float -> Float -> Float
lerp t a b =
  (1 - t) * a + t * b
