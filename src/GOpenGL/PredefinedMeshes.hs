{-# LANGUAGE RecordWildCards #-}

module GOpenGL.PredefinedMeshes (
  backgroundObj, menuBackgroundObj, triangle, square, circle, squareShallow,
  vehicle1, bulletMesh, backGround, squareShallowMesh, menuBackgroundMesh, healthMesh
) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U
import qualified Linear as L
import Linear ((!*!), V3(..))
import qualified Data.HashMap.Strict as HMap
import Data.Maybe

import GOpenGL.Meshes (MeshObject(..), Mesh(..), ShaderResources(..))
import Graphics.GLUtil (asUniform, getUniform, ShaderProgram)
import GCommon.Geometry (translate, rotate, scale)



backgroundObj :: IO MeshObject
backgroundObj = MeshObject <$> vertsVbo <*> (pure draw')
  where
    vertsV = concat [[i, -10, i, 10] | i <- [-10.0, -9.95 .. 10.0]]
    vertsH = concat [[-10, i, 10, i] | i <- [-10.0, -9.95 .. 10.0]]
    verts = vertsV ++ vertsH :: [Float]
    vertsVbo = U.makeBuffer GL.ArrayBuffer verts
    draw' vertsVbo' r = do
      U.enableAttrib (program r) (vertsAttr r)

      GL.bindBuffer GL.ArrayBuffer $= Just vertsVbo'
      U.setAttrib (program r) (vertsAttr r)
        GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0

      GL.drawArrays GL.Lines 0 (fromIntegral $ length verts `div` 2)

      GL.vertexAttribArray (U.getAttrib (program r) (vertsAttr r)) $= GL.Disabled

menuBackgroundObj :: IO MeshObject
menuBackgroundObj = MeshObject <$> vertsVbo <*> (pure draw')
  where
    verts = [1, 1, -1, 1, 1, -1, -1, -1] :: [Float]
    vertsVbo = U.makeBuffer GL.ArrayBuffer verts
    draw' vertsVbo' ShaderResources{..} = do
      asUniform (1 :: GL.GLint) $ getUniform program transparUnif
      U.enableAttrib program vertsAttr

      GL.bindBuffer GL.ArrayBuffer $= Just vertsVbo'
      U.setAttrib program vertsAttr
        GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0

      GL.drawArrays GL.TriangleStrip 0 4

      asUniform (0 :: GL.GLint) $ getUniform program transparUnif
      GL.vertexAttribArray (U.getAttrib program vertsAttr) $= GL.Disabled

triangle :: IO MeshObject
triangle = MeshObject <$> vertsVbo <*> (pure draw')
  where
    verts = [0.0, 0.1, -0.1, -0.1, 0.1, -0.1] :: [Float]
    vertsVbo = U.makeBuffer GL.ArrayBuffer verts
    draw' vertsVbo' r = do
      U.enableAttrib (program r) (vertsAttr r)

      GL.bindBuffer GL.ArrayBuffer $= Just vertsVbo'
      U.setAttrib (program r) (vertsAttr r)
        GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0

      GL.drawArrays GL.Triangles 0 3

      GL.vertexAttribArray (U.getAttrib (program r) (vertsAttr r)) $= GL.Disabled

square :: IO MeshObject
square = MeshObject <$> vertsVbo <*> (pure draw')
  where
    verts = [0.1, 0.1, -0.1, 0.1, 0.1, -0.1, -0.1, -0.1] :: [Float]
    vertsVbo = U.makeBuffer GL.ArrayBuffer verts
    draw' vertsVbo' (ShaderResources prog vAttr _ _ _ _ _ _) = do
      U.enableAttrib prog vAttr

      GL.bindBuffer GL.ArrayBuffer $= Just vertsVbo'
      U.setAttrib prog vAttr
        GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0

      GL.drawArrays GL.TriangleStrip 0 4

      GL.vertexAttribArray (U.getAttrib prog vAttr) $= GL.Disabled


squareShallow :: IO MeshObject
squareShallow = MeshObject <$> vertsVbo <*> (pure draw')
  where
    verts = [0.1, 0.1, -0.1, 0.1, -0.1, -0.1, 0.1, -0.1] :: [Float]
    vertsVbo = U.makeBuffer GL.ArrayBuffer verts
    draw' vertsVbo' (ShaderResources prog vAttr _ _ _ _ _ _) = do
      U.enableAttrib prog vAttr

      GL.bindBuffer GL.ArrayBuffer $= Just vertsVbo'
      U.setAttrib prog vAttr
        GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0

      GL.drawArrays GL.LineLoop 0 4

      GL.vertexAttribArray (U.getAttrib prog vAttr) $= GL.Disabled


circle :: IO MeshObject
circle = MeshObject <$> vertsVbo <*> (pure draw')
  where
    n = 20 :: Int
    n' = 20 :: Float
    verts = concat [[0.1 * cos (i * 2 * pi / n'), 0.1 * sin (i * 2 * pi / n')] | i <- [0 .. n']]
    vertsVbo = U.makeBuffer GL.ArrayBuffer verts
    draw' vertsVbo' (ShaderResources prog vAttr _ _ _ _ _ _) = do
      U.enableAttrib prog vAttr

      GL.bindBuffer GL.ArrayBuffer $= Just vertsVbo'
      U.setAttrib prog vAttr
        GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0

      GL.drawArrays GL.TriangleFan 0 (fromIntegral n)

      GL.vertexAttribArray (U.getAttrib prog vAttr) $= GL.Disabled

vehicle1 :: (HMap.HashMap Int MeshObject) -> IO Mesh -- triangle is 1, square is 2, circle is 3
vehicle1 objectsMap = do
  let circle' = fromJust $ HMap.lookup 3 objectsMap
      square' = fromJust $ HMap.lookup 2 objectsMap
      bigCircle     = MeshLeaf circle' (V3 1 1 0)
      smallCircle   = MeshLeaf circle' (V3 0 0.75 1)
      blackCircle   = MeshLeaf circle' (V3 0 0 0)
      coloredSquare = MeshLeaf square' (V3 0.75 0 1)
      blackSquare   = MeshLeaf square' (V3 0 0 0 )
  let transBlack       = scale 1.15 1.15
      transBlackSmall  = scale 1.3 1.3
      transBlackSquare = scale 1.1 1.3
      transSmallFirst  = (translate $ L.V2 (-0.165) (-0.05)) !*! (scale 0.4 0.4)
      transSmallSecond = (translate $ L.V2 (-0.075) (-0.15)) !*! (scale 0.4 0.4)
      transSmallThird  = (translate $ L.V2 (0.075) (-0.15)) !*! (scale 0.4 0.4)
      transSmallFourth = (translate $ L.V2 (0.165) (-0.05)) !*! (scale 0.4 0.4)
      transSquareOne   = (translate $ L.V2 0 0.20) !*! (scale 0.4 0.4) -- 0.21
      transSquareTwo   = (translate $ L.V2 0 0.12) !*! (scale 1.4 0.4) -- 0.13
      figure =  MeshNode [
        (blackCircle, transBlack), (bigCircle, L.identity),
        (blackCircle, transSmallFirst !*! transBlackSmall), (smallCircle, transSmallFirst),
        (blackCircle, transSmallSecond !*! transBlackSmall), (smallCircle, transSmallSecond),
        (blackCircle, transSmallThird !*! transBlackSmall), (smallCircle, transSmallThird),
        (blackCircle, transSmallFourth !*! transBlackSmall), (smallCircle, transSmallFourth),
        (blackSquare, transSquareOne !*! transBlackSmall), (blackSquare, transSquareTwo !*! transBlackSquare),
        (coloredSquare, transSquareOne), (coloredSquare, transSquareTwo)
        ]
  return $ MeshNode [(figure, rotate (-pi / 2))]

bulletMesh :: (HMap.HashMap Int MeshObject) -> IO Mesh
bulletMesh objectsMap = do
  let circle' = fromJust $ HMap.lookup 3 objectsMap
      mesh    = MeshLeaf circle' (V3 0 0 1)
  let trans = scale 0.3 0.3
  return $ MeshNode [(mesh, trans)]


backGround :: (HMap.HashMap Int MeshObject) -> IO Mesh
backGround objectsMap = do
  let background' = fromJust $ HMap.lookup 0 objectsMap
  return $ MeshLeaf background' (V3 0.78 0.78 0.78)

menuBackgroundMesh :: (HMap.HashMap Int MeshObject) -> IO Mesh
menuBackgroundMesh objectsMap = do
  let menuBackground' = fromJust $ HMap.lookup (-2) objectsMap
  return $ MeshLeaf menuBackground' (V3 0 0 0)

healthMesh :: (HMap.HashMap Int MeshObject) -> IO Mesh
healthMesh objectsMap = do
  let square' = fromJust $ HMap.lookup 2 objectsMap
      mesh    = MeshLeaf square' (V3 0 0 0)
      trans   = scale 2 1
  return $ MeshNode [(mesh, trans)]


squareShallowMesh :: (HMap.HashMap Int MeshObject) -> IO Mesh
squareShallowMesh objectsMap = do
  let squareShallow' = fromJust $ HMap.lookup (-1) objectsMap
      mesh = MeshLeaf squareShallow' (V3 0 0 0 )
      trans = scale 1.8 1.8
  return $ MeshNode [(mesh, trans)]