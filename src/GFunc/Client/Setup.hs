{-# LANGUAGE RecordWildCards #-}

module GFunc.Client.Setup (
  initialSetup,
  shaderResourcesSetup,
  gatherSettings,
  gatherSettingsReset
  ) where

import qualified Network.Socket.ByteString as NSB
import Data.Serialize (decode)
import Text.Printf (printf)
import qualified Data.HashMap.Strict as HMap
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import           System.FilePath ((</>))
import qualified Graphics.GLUtil as U
import Text.Read (readMaybe)
import Control.Monad (when)
import Data.Maybe (fromJust)

import GLogger.Client (logInfo)
import GCommon.Types.Generic (PlayerSettings(..), PlayerSettingsReset(..), ConnHandle(..), ClientKey)
import GState.Client (ClientState(..), newClientState)
import GNetwork.Client (sendMessage)
import GMessages.Network.ClientServer (Message(..), ConnectionMessage(..), ServiceMessage(..))
import qualified GMessages.Network.ServerClient as SC
import GOpenGL.PredefinedMeshes
import GOpenGL.Meshes (ShaderResources(..), Mesh(..), MeshObject(..), MeshObjectMap, MeshMap)



initialSetup :: ConnHandle -> IO ClientState
initialSetup connHandle = do
  settings <- gatherSettings
  _   <- sendMessage connHandle (ServiceMessage 0 $ ConnectionMessage $ ConnectionRequest settings)
  key <- receivePlayerKey connHandle
  logInfo (printf "Key received: %s" (show key))
  newClientState connHandle key


gatherSettings :: IO PlayerSettings
gatherSettings = do
  putStrLn "Enter name:"
  name <- getLine

  team <- getTeam
  vehicle <- getVehicle

  return $ PlayerSettings name team vehicle

gatherSettingsReset :: IO PlayerSettingsReset
gatherSettingsReset = do
  team <- getTeam
  vehicle <- getVehicle

  return $ PlayerSettingsReset team vehicle

getTeam :: IO Int
getTeam = do
  putStrLn "Pick a team\n1 - Blue\n2 - Red"
  teamM <- readMaybe <$> getLine :: IO (Maybe Int)
  case teamM of
    Nothing -> do
      putStrLn "Invalid team, pick again."
      getTeam
    Just team -> if (team /= 1 && team /= 2)
      then do
        putStrLn "Invalid team, pick again."
        getTeam
      else return team

getVehicle :: IO Int
getVehicle = do
  putStrLn "Choose vehicle\n1 - (Low damage, triple shot, fast)\n2 - (Medium damage, double shot)\n3 - (High damage, single shot, slow)"
  vehicleM <- readMaybe <$> getLine :: IO (Maybe Int)
  case vehicleM of
    Nothing -> do
      putStrLn "Invalid vehicle, pick again."
      getVehicle
    Just vehicle -> if (1 > vehicle || 3 < vehicle)
      then do
        putStrLn "Invalid vehicle, pick again."
        getVehicle
      else return vehicle

receivePlayerKey :: ConnHandle -> IO ClientKey
receivePlayerKey connHandle@ConnHandle{..} = do
  recv <- NSB.recv connSocket maxBytes
  let eitherMessage = decode recv
  case eitherMessage of
    Right message -> case message of
      SC.PlayerKey key -> return key
    Left _        -> receivePlayerKey connHandle
 where
  maxBytes = 1024

meshMapSetup :: MeshObjectMap -> IO MeshMap
meshMapSetup objectsMap = do
  background'  <- backGroundMesh objectsMap
  menuBackgroundMesh' <- menuBackgroundMesh objectsMap

  vehicle1Red' <- vehicle1RedMesh objectsMap
  vehicle1Blue' <- vehicle1BlueMesh objectsMap
  bullet1RedMesh' <- bullet1RedMesh objectsMap
  bullet1BlueMesh' <- bullet1BlueMesh objectsMap

  vehicle2Red' <- vehicle2RedMesh objectsMap
  vehicle2Blue' <- vehicle2BlueMesh objectsMap
  bullet2RedMesh' <- bullet2RedMesh objectsMap
  bullet2BlueMesh' <- bullet2BlueMesh objectsMap

  vehicle3Red' <- vehicle3RedMesh objectsMap
  vehicle3Blue' <- vehicle3BlueMesh objectsMap
  bullet3RedMesh' <- bullet3RedMesh objectsMap
  bullet3BlueMesh' <- bullet3BlueMesh objectsMap

  squareShallowMesh' <- squareShallowMesh objectsMap
  healthMeshBackRed' <- healthBackRedMesh objectsMap
  healthMeshFrontRed' <- healthFrontRedMesh objectsMap
  healthMeshBackBlue' <- healthBackBlueMesh objectsMap
  healthMeshFrontBlue' <- healthFrontBlueMesh objectsMap
  minimap' <- minimapMesh objectsMap
  minimapDotRed' <- minimapDotRed objectsMap
  minimapDotBlue' <- minimapDotBlue objectsMap
  minimapDotGreen' <- minimapDotGreen objectsMap
  let meshMap = (HMap.insert "background" background') .
                (HMap.insert "squareShallow" squareShallowMesh') .
                (HMap.insert "menuBackground" menuBackgroundMesh') .
                (HMap.insert "healthBackRed" healthMeshBackRed') .
                (HMap.insert "healthFrontRed" healthMeshFrontRed') .
                (HMap.insert "healthBackBlue" healthMeshBackBlue') .
                (HMap.insert "healthFrontBlue" healthMeshFrontBlue') .
                (HMap.insert "minimap" minimap') .
                (HMap.insert "minimapDotRed" minimapDotRed') .
                (HMap.insert "minimapDotBlue" minimapDotBlue') .
                (HMap.insert "minimapDotGreen" minimapDotGreen') .
                (HMap.insert "vehicle1Red" vehicle1Red') .
                (HMap.insert "vehicle1Blue" vehicle1Blue') .
                (HMap.insert "bullet1Red" bullet1RedMesh') .
                (HMap.insert "bullet1Blue" bullet1BlueMesh') .
                (HMap.insert "vehicle2Red" vehicle2Red') .
                (HMap.insert "vehicle2Blue" vehicle2Blue') .
                (HMap.insert "bullet2Red" bullet2RedMesh') .
                (HMap.insert "bullet2Blue" bullet2BlueMesh') .
                (HMap.insert "vehicle3Red" vehicle3Red') .
                (HMap.insert "vehicle3Blue" vehicle3Blue') .
                (HMap.insert "bullet3Red" bullet3RedMesh') .
                (HMap.insert "bullet3Blue" bullet3BlueMesh') $ HMap.empty
  return meshMap

meshObjectsMapSetup :: IO MeshObjectMap
meshObjectsMapSetup = do
  background' <- backgroundMeshObj
  triangle' <- triangleMeshObj
  square'   <- squareMeshObj
  squareTransparent' <- squareTransparentMeshObj
  circle'   <- circleMeshObj
  circleTransparent' <- circleTransparentMeshObj
  squareShallow' <- squareShallowMeshObj
  let meshObjectsMap = (HMap.insert "background" background') .
                       (HMap.insert "triangle" triangle') .
                       (HMap.insert "square" square') .
                       (HMap.insert "squareTransparent" squareTransparent') .
                       (HMap.insert "circle" circle') .
                       (HMap.insert "circleTransparent" circleTransparent') .
                       (HMap.insert "squareShallow" squareShallow') $ HMap.empty
  return meshObjectsMap

shaderResourcesSetup :: IO ShaderResources
shaderResourcesSetup = do
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.multisample $= GL.Enabled
  let v = "Shaders" </> "vertex_shader.glsl"
      f = "Shaders" </> "fragment_shader.glsl"
  meshObjectsMap' <- meshObjectsMapSetup
  ShaderResources <$> U.simpleShaderProgram v f
            <*> pure "a_verts"
            <*> pure "u_color"
            <*> pure "u_transform"
            <*> pure "u_projection"
            <*> pure "u_transparent"
            <*> meshMapSetup meshObjectsMap'
            <*> pure meshObjectsMap'
