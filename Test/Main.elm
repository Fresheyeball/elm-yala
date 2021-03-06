module Main (..) where

import ElmTest as T exposing (Test,consoleRunner)
import Check.Test exposing (..)
import Check.Investigator exposing (..)
import Shrink exposing (Shrinker)
import Math.Vec2 as V exposing (Vec2)
import Math.Interpolate as I
import Random.Extra as R
import Random
import Nearly exposing ((~=))
import Console
import Task


cycles : Int
cycles = 1000


seed : Random.Seed
seed = Random.initialSeed 666


vec2 : Investigator Vec2
vec2 =
  let
    shrinker { x, y } =
      let
        (<$) =
          Shrink.map

        (<*) =
          Shrink.andMap
      in
        Vec2 <$ shrink float x <* shrink float y

    generator =
      let
        (<$) =
          R.map

        (<*) =
          R.andMap
      in
        Vec2 <$ random float <* random float
  in
    investigator generator shrinker


invert : Test
invert =
  let
    (<+>) =
      V.add
  in
    assert
      "invert"
      (\a -> a <+> V.neg a ~ Vec2 0 0)
      vec2
      cycles
      seed


distributive : Test
distributive =
  let
    (<+>) =
      V.add

    (**) =
      V.scale
  in
    assert3
      "distributive"
      (\r p q -> r ** (p <+> q) ~ (r ** p) <+> (r ** q))
      float
      vec2
      vec2
      cycles
      seed


distributiveScalar : Test
distributiveScalar =
  let
    (<+>) =
      V.add

    (**) =
      V.scale
  in
    assert3
      "distributive scalar"
      (\r s p -> ((r + s) ** p) ~ ((r ** p) <+> (s ** p)))
      float
      float
      vec2
      cycles
      seed


associativeScalar : Test
associativeScalar =
  let
    (**) =
      V.scale
  in
    assert3
      "associative scalar"
      (\r s p -> r ** (s ** p) ~ (r * s) ** p)
      float
      float
      vec2
      cycles
      seed


infixr 6 ~
(~) : Vec2 -> Vec2 -> Bool
(~) a b =
  a.x ~= b.x && a.y ~= b.y


multiplicativeMonoid : Test
multiplicativeMonoid =
  let
    (<*>) =
      V.mul

    (**) =
      V.scale

    one =
      Vec2 1 1

    id a =
      (one <*> a) ~ a && a ~ (a <*> one)

    associative a b c =
      a <*> (b <*> c) ~ (a <*> b) <*> c

    scaleIdenity a =
      1 ** a ~ a
  in
    assert3
      "Multiplicative Monoid"
      (\a b c -> id a && scaleIdenity a && associative a b c)
      vec2
      vec2
      vec2
      cycles
      seed


additiveMonoid : Test
additiveMonoid =
  let
    (<+>) =
      V.add

    commutative a b =
      a <+> b ~ b <+> a

    zero =
      Vec2 0 0

    id a =
      zero <+> a ~ a && a ~ a <+> zero

    associative a b c =
      a <+> (b <+> c) ~ (a <+> b) <+> c
  in
    assert3
      "Additive Monoid"
      (\a b c -> commutative a b && id a && associative a b c)
      vec2
      vec2
      vec2
      cycles
      seed


rangeFloat : Float -> Float -> Investigator Float
rangeFloat min max =
  investigator (Random.float min max) Shrink.float


algebra : Test
algebra =
  T.suite
    "Vec2 Algebric Properties"
    [ invert
    , additiveMonoid
    , multiplicativeMonoid
    , distributive
    , associativeScalar
    , distributiveScalar
    ]


distanceAndMagnitude : Test
distanceAndMagnitude =
    let
        (<->) =
            V.sub

        dIsMfromZ a =
            V.distance V.zero a ~= V.magnitude a

        morgan a b =
            abs (V.distance a b) ~= V.magnitude (a <-> b)

    in
    assert2
        "distance and magnitude"
        (\a b -> dIsMfromZ a && morgan a b)
        vec2
        vec2
        cycles
        seed

directionAndMagnitude : Test
directionAndMagnitude =
    let
        (**) =
            V.scale

        (<+>) =
            V.add

        (<->) =
            V.sub

        morgan v u =
            v <+> (V.magnitude (v <-> u) ** V.direction u v) ~ u

    in
    assert2
        "direction and magnitude"
        morgan
        vec2
        vec2
        cycles
        seed



lerp : Test
lerp =
    let
        bounds a b =
            I.lerp 1 a b ~= b && I.lerp 0 a b ~= a

        id a =
            I.lerp a 0 1 ~= a

        morgan a b c d =
               (I.lerp c a b + I.lerp d a b)
            ~= (I.lerp (c + d) a b + a)

        derivative a b d w x =
               (I.lerp b w a - I.lerp b (w + d) a)
            ~= (I.lerp b x a - I.lerp b (x + d) a)

    in
      assert5
        "lerp"
        (\a b d x y -> bounds a b && id a && morgan a b x y && derivative a b d x y)
        float
        float
        float
        (rangeFloat 0.0 1.0)
        (rangeFloat 0.0 1.0)
        cycles
        seed


port runner : Signal (Task.Task x ())
port runner =
  Console.run <| consoleRunner <| T.suite
    "All tests"
    [algebra, lerp, distanceAndMagnitude, directionAndMagnitude]
