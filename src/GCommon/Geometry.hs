{-# LANGUAGE DeriveGeneric #-}

module GCommon.Geometry (
  Point, Radius, Angle,
  Rectangle(..),
  insideRectangle,
  translateRectangle,
  translate,
  rotate,
  scale
) where

import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Control.DeepSeq (NFData)
import Graphics.Rendering.OpenGL (GLfloat)
import Linear (M33, V3(..), V2(..), (!*))


type Point  = (GLfloat, GLfloat)
type Radius = GLfloat
type Angle  = GLfloat


data Rectangle = Rectangle {
  x1 :: GLfloat,
  y1 :: GLfloat,
  x2 :: GLfloat,
  y2 :: GLfloat
} deriving (Show, Eq, Generic)


translateRectangle :: Rectangle -> V2 GLfloat -> Rectangle
translateRectangle (Rectangle x1 y1 x2 y2) (V2 a b) = Rectangle (x1 + a) (y1 + b) (x2 + a) (y2 + b)

insideRectangle :: Rectangle -> V2 GLfloat -> Bool
insideRectangle (Rectangle x1 y1 x2 y2) (V2 a b) = (x1 < a && a < x2 && b < y1 && y2 < b)

instance Serialize Rectangle
instance NFData Rectangle

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