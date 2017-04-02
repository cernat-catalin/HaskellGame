module Common.GTypes (
  ClientName,
  Message(..)
  ) where


type ClientName = String

data Message = RandomObject
             | ConnectionRequest ClientName
             | InvalidName