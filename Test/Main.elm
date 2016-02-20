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


port runner : Signal (Task.Task x ())
port runner =
  Console.run (consoleRunner algebra)
