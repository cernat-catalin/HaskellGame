module Common.GTransform (
  getPlayer,
  updatePlayer,
  movePlayerLeft,
  movePlayerRight,
  movePlayerUp,
  movePlayerDown,
  addPlayer,
  removePlayer
  ) where

import Control.Monad.State (get, modify, execState)
import qualified Data.Map as Map
import Network.Socket (SockAddr)

import Common.GObjects (Circle(..), CircleS, Player(..), PlayerS, World(..), WorldS)

getPlayer :: SockAddr -> WorldS (Maybe Player)
getPlayer addr = get >>= (return . (Map.lookup addr) . players)

updatePlayer :: SockAddr -> PlayerS () -> WorldS ()
updatePlayer addr upFunc = getPlayer addr >>=
  \plyM -> case plyM of
    Nothing  -> return ()
    Just ply' -> modify (\world -> world {players = Map.insert (clientKey ply') (execState upFunc ply') (players world)})

addPlayer :: SockAddr -> WorldS ()
addPlayer addr = modify (\world -> world {players = Map.insert addr newPlayer (players world)})
 where
  newPlayer = Player addr newCircle
  newCircle = Circle (0, 0) 0.1

removePlayer :: SockAddr -> WorldS ()
removePlayer addr = modify (\world -> world {players = Map.delete addr (players world)})

moveCircle :: (Double, Double) ->  CircleS ()
moveCircle (dx, dy)= modify (\(Circle (x, y) rad) -> Circle (x + dx, y + dy) rad)

movePlayerLeft :: PlayerS ()
movePlayerLeft = do
  ply <- get
  let circle' = execState (moveCircle (-0.1, 0)) (circle ply)
  modify (\ply' -> ply' {circle = circle'})

movePlayerRight :: PlayerS ()
movePlayerRight = do
  ply <- get
  let circle' = execState (moveCircle (0.1, 0)) (circle ply)
  modify (\ply' -> ply' {circle = circle'})

movePlayerUp :: PlayerS ()
movePlayerUp = do
  ply <- get
  let circle' = execState (moveCircle (0, 0.1)) (circle ply)
  modify (\ply' -> ply' {circle = circle'})

movePlayerDown :: PlayerS ()
movePlayerDown = do
  ply <- get
  let circle' = execState (moveCircle (0, -0.1)) (circle ply)
  modify (\ply' -> ply' {circle = circle'})