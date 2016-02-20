module Math.Interpolate (..) where

{-|
@docs lerp
-}

{-|
Interpolate the space between 2 values. For example consider
the int range from 10 to 20. A `lerp 10 20 0.5 == 15` as `15` is half way (`0.5`) between `10` and `20`


-}
lerp : Float -> Float -> Float -> Float
lerp a b t =
  (1 - t) * a + t * b
