{-# LANGUAGE DeriveGeneric #-}

module GCommon.Geometry (
  Point, Radius, Angle,
  Rectangle(..),
  translate,
  rotate,
  scale
) where

import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Graphics.Rendering.OpenGL (GLfloat)
import Linear (M33, V3(..), V2(..))


type Point  = (GLfloat, GLfloat)
type Radius = GLfloat
type Angle  = GLfloat


data Rectangle = Rectangle {
  x1 :: GLfloat,
  y1 :: GLfloat,
  x2 :: GLfloat,
  y2 :: GLfloat
} deriving (Show, Eq, Generic)

instance Serialize Rectangle

translate :: V2 GLfloat -> M33 GLfloat
translate (V2 x y) = V3
  (V3 1 0 x)
  (V3 0 1 y)
  (V3 0 0 1)

rotate :: GLfloat -> M33 GLfloat
rotate r = V3
  (V3 (cos r) (-sin r) 0)
  (V3 (sin r) (cos r) 0)
  (V3 0 0 1)

scale :: GLfloat -> GLfloat -> M33 GLfloat
scale x y = V3
  (V3 x 0 0)
  (V3 0 y 0)
  (V3 0 0 1)