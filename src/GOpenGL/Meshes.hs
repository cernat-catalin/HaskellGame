module GOpenGL.Meshes (
  ShaderResources(..),
  MeshObject(..),
  Mesh(..),
  Color,
  drawMesh
  ) where

import Graphics.Rendering.OpenGL (BufferObject, GLfloat)
import Graphics.GLUtil (asUniform, getUniform, ShaderProgram)
import Linear (V3, M33, (!*!))
import qualified Data.HashMap.Strict as HMap

import Text.Printf (printf)
import GLogger.Client (logInfo)


data ShaderResources = ShaderResources {
  program       :: ShaderProgram,
  vertsAttr     :: String,
  colorUnif     :: String,
  transformUnif :: String,
  meshMap       :: HMap.HashMap Int Mesh,
  meshObjectsMap :: HMap.HashMap Int MeshObject
}

data MeshObject = MeshObject {
  vertsVbo  :: BufferObject,
  draw      :: BufferObject -> ShaderResources -> IO ()
}

type Color = V3 GLfloat

data Mesh = MeshLeaf MeshObject Color
          | MeshNode [(Mesh, M33 GLfloat)]

drawMesh :: ShaderResources -> M33 GLfloat -> Mesh -> IO ()
drawMesh r trans mesh = drawMesh' r trans mesh 

drawMesh' :: ShaderResources -> M33 GLfloat -> Mesh -> IO ()
drawMesh' r trans mesh = case mesh of
  MeshLeaf meshObject color -> do
    asUniform trans $ getUniform (program r) (transformUnif r)
    asUniform color $ getUniform (program r) (colorUnif r)
    draw meshObject (vertsVbo meshObject) r
  MeshNode meshes -> do
    mapM_ (\(mesh', trans') -> drawMesh' r (trans !*! trans') mesh') meshes


