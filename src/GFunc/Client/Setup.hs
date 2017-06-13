{-# LANGUAGE RecordWildCards #-}

module GFunc.Client.Setup (
  initialSetup,
  shaderResourcesSetup
  ) where

import qualified Network.Socket.ByteString as NSB
import Data.Serialize (decode)
import Text.Printf (printf)
import qualified Data.HashMap.Strict as HMap
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import           System.FilePath ((</>))
import qualified Graphics.GLUtil as U

import GLogger.Client (logInfo)
import GCommon.Types.Generic (ClientSettings(..), ConnHandle(..), ClientKey)
import GState.Client (ClientState(..), newClientState)
import GNetwork.Client (sendMessage)
import GMessages.Network.ClientServer (Message(..), ConnectionMessage(..), ServiceMessage(..))
import qualified GMessages.Network.ServerClient as SC
import GOpenGL.PredefinedMeshes
import GOpenGL.Meshes (ShaderResources(..), Mesh(..), MeshObject(..))



initialSetup :: ConnHandle -> IO ClientState
initialSetup connHandle = do
  let settings = ClientSettings {name = "Levi", color = "Green"}
  _   <- sendMessage connHandle (ServiceMessage $ ConnectionMessage $ ConnectionRequest settings)
  key <- receivePlayerKey connHandle
  logInfo (printf "Key received: %s" (show key))
  newClientState connHandle key

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


meshMapSetup :: (HMap.HashMap Int MeshObject) -> IO (HMap.HashMap Int Mesh)
meshMapSetup objectsMap = do
  background'  <- backGround objectsMap
  bulletMesh' <- bulletMesh objectsMap
  firstEdina' <- firstEdina objectsMap
  squareShallowMesh' <- squareShallowMesh objectsMap
  let meshMap = (HMap.insert 0 background') .
                (HMap.insert 1 bulletMesh') .
                (HMap.insert (-1) squareShallowMesh') .
                (HMap.insert 2 firstEdina') $ HMap.empty
  return meshMap

meshObjectsMapSetup :: IO (HMap.HashMap Int MeshObject)
meshObjectsMapSetup = do
  background' <- backgroundObj
  triangle' <- triangle
  square'   <- square
  circle'   <- circle
  squareShallow' <- squareShallow
  let meshObjectsMap = (HMap.insert 0 background') .
                       (HMap.insert 1 triangle') .
                       (HMap.insert 2 square') .
                       (HMap.insert (-1) squareShallow') .
                       (HMap.insert 3 circle') $ HMap.empty
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
            <*> meshMapSetup meshObjectsMap'
            <*> pure meshObjectsMap'