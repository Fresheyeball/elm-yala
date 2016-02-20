module Math.Interpolate (..) where


lerp : Float -> Float -> Float -> Float
lerp a b t =
  (1 - t) * a + t * b
