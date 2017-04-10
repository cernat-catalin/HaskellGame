{-# LANGUAGE DeriveGeneric #-}

module Common.GObjects (
  Circle(..),
  Point,
  Radius
  ) where

import Graphics.Rendering.OpenGL (GLdouble)
import GHC.Generics (Generic)
import Data.Serialize (Serialize)


type Point = (GLdouble, GLdouble)

type Radius = GLdouble

data Circle = Circle {
  center :: Point,
  radius :: Radius
} deriving (Show, Generic)

instance Serialize Circle