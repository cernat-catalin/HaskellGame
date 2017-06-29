{-# LANGUAGE RecordWildCards #-}

module GOpenGL.PredefinedMeshes (
  backgroundMeshObj, triangleMeshObj, squareMeshObj, circleMeshObj, squareShallowMeshObj, squareTransparentMeshObj, circleTransparentMeshObj,
  vehicle1RedMesh, vehicle1BlueMesh, vehicle2RedMesh, vehicle2BlueMesh, vehicle3RedMesh, vehicle3BlueMesh,
  bullet1RedMesh, bullet1BlueMesh, bullet2RedMesh, bullet2BlueMesh, bullet3RedMesh, bullet3BlueMesh,
  backGroundMesh, squareShallowMesh, menuBackgroundMesh,
  healthBackRedMesh, healthFrontRedMesh,
  healthBackBlueMesh, healthFrontBlueMesh,
  minimapMesh, minimapDotBlue, minimapDotRed, minimapDotGreen
) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U
import qualified Linear as L
import Linear ((!*!), V3(..))
import qualified Data.HashMap.Strict as HMap
import Data.Maybe

import GOpenGL.Meshes (MeshObject(..), Mesh(..), ShaderResources(..), MeshObjectMap)
import Graphics.GLUtil (asUniform, getUniform, ShaderProgram)
import GCommon.Geometry (translate, rotate, scale)



backgroundMeshObj :: IO MeshObject
backgroundMeshObj = MeshObject <$> vertsVbo <*> (pure draw')
  where
    w = 5
    h = 5
    vertsV = concat [[i, -w, i, w] | i <- [-w, -(w - 0.05) .. w]]
    vertsH = concat [[-w, i, h, i] | i <- [-h, -(h - 0.05) .. h]]
    verts = vertsV ++ vertsH :: [Float]
    vertsVbo = U.makeBuffer GL.ArrayBuffer verts
    draw' vertsVbo' r = do
      U.enableAttrib (program r) (vertsAttr r)

      GL.bindBuffer GL.ArrayBuffer $= Just vertsVbo'
      U.setAttrib (program r) (vertsAttr r)
        GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0

      GL.drawArrays GL.Lines 0 (fromIntegral $ length verts `div` 2)

      GL.vertexAttribArray (U.getAttrib (program r) (vertsAttr r)) $= GL.Disabled

triangleMeshObj :: IO MeshObject
triangleMeshObj = MeshObject <$> vertsVbo <*> (pure draw')
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

squareMeshObj :: IO MeshObject
squareMeshObj = MeshObject <$> vertsVbo <*> (pure draw')
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

squareTransparentMeshObj :: IO MeshObject
squareTransparentMeshObj = MeshObject <$> vertsVbo <*> (pure draw')
  where
    verts = [0.1, 0.1, -0.1, 0.1, 0.1, -0.1, -0.1, -0.1] :: [Float]
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


squareShallowMeshObj :: IO MeshObject
squareShallowMeshObj = MeshObject <$> vertsVbo <*> (pure draw')
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


circleMeshObj :: IO MeshObject
circleMeshObj = MeshObject <$> vertsVbo <*> (pure draw')
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

circleTransparentMeshObj :: IO MeshObject
circleTransparentMeshObj = MeshObject <$> vertsVbo <*> (pure draw')
  where
    n = 20 :: Int
    n' = 20 :: Float
    verts = concat [[0.1 * cos (i * 2 * pi / n'), 0.1 * sin (i * 2 * pi / n')] | i <- [0 .. n']]
    vertsVbo = U.makeBuffer GL.ArrayBuffer verts
    draw' vertsVbo' ShaderResources{..} = do
      asUniform (1 :: GL.GLint) $ getUniform program transparUnif
      U.enableAttrib program vertsAttr

      GL.bindBuffer GL.ArrayBuffer $= Just vertsVbo'
      U.setAttrib program vertsAttr
        GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0

      GL.drawArrays GL.TriangleFan 0 (fromIntegral n)

      asUniform (0 :: GL.GLint) $ getUniform program transparUnif
      GL.vertexAttribArray (U.getAttrib program vertsAttr) $= GL.Disabled

vehicle1RedMesh :: MeshObjectMap -> IO Mesh
vehicle1RedMesh objectsMap = do
  let circle' = fromJust $ HMap.lookup "circle" objectsMap
      square' = fromJust $ HMap.lookup "square" objectsMap
      bigCircle     = MeshLeaf circle' (V3 1 0 0)
      smallCircle   = MeshLeaf circle' (V3 0.75 0 1)
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

vehicle1BlueMesh :: MeshObjectMap -> IO Mesh
vehicle1BlueMesh objectsMap = do
  let circle' = fromJust $ HMap.lookup "circle" objectsMap
      square' = fromJust $ HMap.lookup "square" objectsMap
      bigCircle     = MeshLeaf circle' (V3 0 0 1)
      smallCircle   = MeshLeaf circle' (V3 0.75 0 1)
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

bullet1BlueMesh :: MeshObjectMap -> IO Mesh
bullet1BlueMesh objectsMap = do
  let circle' = fromJust $ HMap.lookup "circle" objectsMap
      mesh    = MeshLeaf circle' (V3 0 0 1)
  let trans = scale 0.3 0.3
  return $ MeshNode [(mesh, trans)]

bullet1RedMesh :: MeshObjectMap -> IO Mesh
bullet1RedMesh objectsMap = do
  let circle' = fromJust $ HMap.lookup "circle" objectsMap
      mesh    = MeshLeaf circle' (V3 1 0 0)
  let trans = scale 0.3 0.3
  return $ MeshNode [(mesh, trans)]


vehicle2RedMesh :: MeshObjectMap -> IO Mesh
vehicle2RedMesh objectsMap = do
  let circle' = fromJust $ HMap.lookup "circle" objectsMap
      square' = fromJust $ HMap.lookup "square" objectsMap
      circle = MeshLeaf circle' (V3 1 0 0)
      blackCircle = MeshLeaf circle' (V3 0 0 0)
      cannon = MeshLeaf square' (V3 0.75 0 1)
      blackCannon = MeshLeaf square' (V3 0 0 0)
  let transCircle = scale 1.5 1.5
      transBlackCircle = scale 1.65 1.65
      transCannon = (translate $ L.V2 0 (0.25)) !*! (scale 1.5 2)
      transBlackCannon = scale 1.10 1.07
      figure = MeshNode [
        (blackCannon, transCannon !*! transBlackCannon), (cannon, transCannon),
        (blackCircle, transBlackCircle), (circle, transCircle)
        ]
  return $ MeshNode [(figure, rotate (-pi/2))]

vehicle2BlueMesh :: MeshObjectMap -> IO Mesh
vehicle2BlueMesh objectsMap = do
  let circle' = fromJust $ HMap.lookup "circle" objectsMap
      square' = fromJust $ HMap.lookup "square" objectsMap
      circle = MeshLeaf circle' (V3 0 0 1)
      blackCircle = MeshLeaf circle' (V3 0 0 0)
      cannon = MeshLeaf square' (V3 0.75 0 1)
      blackCannon = MeshLeaf square' (V3 0 0 0)
  let transCircle = scale 1.5 1.5
      transBlackCircle = scale 1.65 1.65
      transCannon = (translate $ L.V2 0 (0.25)) !*! (scale 1.5 2)
      transBlackCannon = scale 1.10 1.07
      figure = MeshNode [
        (blackCannon, transCannon !*! transBlackCannon), (cannon, transCannon),
        (blackCircle, transBlackCircle), (circle, transCircle)
        ]
  return $ MeshNode [(figure, rotate (-pi/2))]

bullet2BlueMesh :: MeshObjectMap -> IO Mesh
bullet2BlueMesh objectsMap = do
  let circle' = fromJust $ HMap.lookup "circle" objectsMap
      mesh    = MeshLeaf circle' (V3 0 0 1)
  let trans = scale 1.5 1.5
  return $ MeshNode [(mesh, trans)]

bullet2RedMesh :: MeshObjectMap -> IO Mesh
bullet2RedMesh objectsMap = do
  let circle' = fromJust $ HMap.lookup "circle" objectsMap
      mesh    = MeshLeaf circle' (V3 1 0 0)
  let trans = scale 1.5 1.5
  return $ MeshNode [(mesh, trans)]

vehicle3RedMesh :: MeshObjectMap -> IO Mesh
vehicle3RedMesh objectsMap = do
  let circle' = fromJust $ HMap.lookup "circle" objectsMap
      square' = fromJust $ HMap.lookup "square" objectsMap
      circle  = MeshLeaf circle' (V3 1 0 0)
      blackCircle = MeshLeaf circle' (V3 0 0 0)
      cannon = MeshLeaf square' (V3 0.75 0 1)
      blackCannon = MeshLeaf square' (V3 0 0 0)
  let transCircle = scale 1.5 1.5
      transBlackCircle  = scale 1.65 1.65
      transCannon1 = (translate $ L.V2 (-0.0875) 0.15) !*! (scale 0.625 1.5)
      transCannon2 = (translate $ L.V2 0.0875 0.15) !*! (scale 0.625 1.5)
      transBlackCannon = scale 1.15 1.05
      figure = MeshNode [
        (blackCannon, transCannon1 !*! transBlackCannon), (cannon, transCannon1),
        (blackCannon, transCannon2 !*! transBlackCannon), (cannon, transCannon2),
        (blackCircle, transBlackCircle), (circle, transCircle)
        ]
  return $ MeshNode [(figure, rotate (-pi/2))]

vehicle3BlueMesh :: MeshObjectMap -> IO Mesh
vehicle3BlueMesh objectsMap = do
  let circle' = fromJust $ HMap.lookup "circle" objectsMap
      square' = fromJust $ HMap.lookup "square" objectsMap
      circle  = MeshLeaf circle' (V3 0 0 1)
      blackCircle = MeshLeaf circle' (V3 0 0 0)
      cannon = MeshLeaf square' (V3 0.75 0 1)
      blackCannon = MeshLeaf square' (V3 0 0 0)
  let transCircle = scale 1.5 1.5
      transBlackCircle  = scale 1.65 1.65
      transCannon1 = (translate $ L.V2 (-0.0875) 0.15) !*! (scale 0.625 1.5)
      transCannon2 = (translate $ L.V2 0.0875 0.15) !*! (scale 0.625 1.5)
      transBlackCannon = scale 1.15 1.05
      figure = MeshNode [
        (blackCannon, transCannon1 !*! transBlackCannon), (cannon, transCannon1),
        (blackCannon, transCannon2 !*! transBlackCannon), (cannon, transCannon2),
        (blackCircle, transBlackCircle), (circle, transCircle)
        ]
  return $ MeshNode [(figure, rotate (-pi/2))]

bullet3BlueMesh :: MeshObjectMap -> IO Mesh
bullet3BlueMesh objectsMap = do
  let circle' = fromJust $ HMap.lookup "circle" objectsMap
      mesh    = MeshLeaf circle' (V3 0 0 1)
  let trans = scale 0.625 0.625
  return $ MeshNode [(mesh, trans)]

bullet3RedMesh :: MeshObjectMap -> IO Mesh
bullet3RedMesh objectsMap = do
  let circle' = fromJust $ HMap.lookup "circle" objectsMap
      mesh    = MeshLeaf circle' (V3 1 0 0)
  let trans = scale 0.625 0.625
  return $ MeshNode [(mesh, trans)]


backGroundMesh :: MeshObjectMap -> IO Mesh
backGroundMesh objectsMap = do
  let background' = fromJust $ HMap.lookup "background" objectsMap
  return $ MeshLeaf background' (V3 0.78 0.78 0.78)

menuBackgroundMesh :: MeshObjectMap -> IO Mesh
menuBackgroundMesh objectsMap = do
  let menuBackground' = fromJust $ HMap.lookup "squareTransparent" objectsMap
      trans = scale 10 10
      mesh = MeshLeaf menuBackground' (V3 0 0 0)
  return $ MeshNode [(mesh, trans)]
      

healthBackRedMesh :: MeshObjectMap -> IO Mesh
healthBackRedMesh objectsMap = do
  let square' = fromJust $ HMap.lookup "squareShallow" objectsMap
      mesh    = MeshLeaf square' (V3 1 0 0)
      trans   = scale 1.5 0.5
  return $ MeshNode [(mesh, trans)]

healthFrontRedMesh :: MeshObjectMap -> IO Mesh
healthFrontRedMesh objectsMap = do
  let square' = fromJust $ HMap.lookup "square" objectsMap
      mesh    = MeshLeaf square' (V3 1 0 0)
      trans   = scale 1.5 0.5
  return $ MeshNode [(mesh, trans)]

healthBackBlueMesh :: MeshObjectMap -> IO Mesh
healthBackBlueMesh objectsMap = do
  let square' = fromJust $ HMap.lookup "squareShallow" objectsMap
      mesh    = MeshLeaf square' (V3 0 0 1)
      trans   = scale 1.5 0.5
  return $ MeshNode [(mesh, trans)]

healthFrontBlueMesh :: MeshObjectMap -> IO Mesh
healthFrontBlueMesh objectsMap = do
  let square' = fromJust $ HMap.lookup "square" objectsMap
      mesh    = MeshLeaf square' (V3 0 0 1)
      trans   = scale 1.5 0.5
  return $ MeshNode [(mesh, trans)]


squareShallowMesh :: MeshObjectMap -> IO Mesh
squareShallowMesh objectsMap = do
  let squareShallow' = fromJust $ HMap.lookup "squareShallow" objectsMap
      mesh = MeshLeaf squareShallow' (V3 0 0 0)
      trans = scale 1.8 1.8
  return $ MeshNode [(mesh, trans)]

minimapMesh :: MeshObjectMap -> IO Mesh
minimapMesh objectsMap = do
  let square' = fromJust $ HMap.lookup "square" objectsMap
      squareShallow' = fromJust $ HMap.lookup "squareShallow" objectsMap
      mesh1 = MeshLeaf square' (V3 1 1 1)
      mesh2 = MeshLeaf squareShallow' (V3 0.19 0.19 0.19)
      trans = scale 2 2
  return $ MeshNode [(mesh1, trans), (mesh2, trans)]

minimapDotRed :: MeshObjectMap -> IO Mesh
minimapDotRed objectsMap = do
  let circle' = fromJust $ HMap.lookup "circleTransparent" objectsMap
      mesh    = MeshLeaf circle' (V3 1 0 0)
  let trans = scale 0.2 0.2
  return $ MeshNode [(mesh, trans)]

minimapDotBlue :: MeshObjectMap -> IO Mesh
minimapDotBlue objectsMap = do
  let circle' = fromJust $ HMap.lookup "circleTransparent" objectsMap
      mesh    = MeshLeaf circle' (V3 0 0 1)
  let trans = scale 0.2 0.2
  return $ MeshNode [(mesh, trans)]

minimapDotGreen :: MeshObjectMap -> IO Mesh
minimapDotGreen objectsMap = do
  let circle' = fromJust $ HMap.lookup "circleTransparent" objectsMap
      mesh    = MeshLeaf circle' (V3 0 1 0)
  let trans = scale 0.2 0.2
  return $ MeshNode [(mesh, trans)]