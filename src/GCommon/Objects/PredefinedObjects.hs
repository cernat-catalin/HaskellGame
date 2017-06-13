module GCommon.Objects.PredefinedObjects (
  edina1
) where


import Linear (V2(..))

import GCommon.Geometry (Rectangle(..))
import GCommon.Objects.Objects as GO


edina1 :: Vehicle
edina1 = Vehicle {
  _position = V2 0 0,
  _orientation = 0,
  _meshId    = 2,
  _bounding  = Rectangle 0 0 0 0,

  _vWeapons = [
      (Weapon (V2 0 0.20) 0),
      (Weapon (V2 0.11 0.12) ((-pi) / 2)),
      (Weapon (V2 (-0.11) 0.12) (pi / 2))
  ],

  _health    = 0,
  _speed     = 0.01,
  _damage    = 0
}