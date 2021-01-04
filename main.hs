import           Graphics.UI.GLUT
import           Control.Monad

data Hexagon = Hexagon { x :: GLfloat
                       , y :: GLfloat
                       , r :: GLfloat
                       , c :: (GLfloat, GLfloat, GLfloat)
                       } deriving (Show)

myPoints :: GLfloat -> GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
myPoints r deltaX deltaY = let n = 6 
                           in  [ (sin (2 * pi * k / n + pi / n) * r + deltaX, cos (2 * pi * k / n + pi / n) * r + deltaY, 0) | k <- [1 .. n] ]

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

renderHex :: Hexagon -> IO ()
renderHex (Hexagon x y r _) = renderPrimitive LineLoop $ mapM_ vertex3f $ myPoints r x y

generateLattice :: [Hexagon]
generateLattice = odd ++ even
                  where deltaY = 0.1 * sin (2 * pi / 6) * 2 :: GLfloat
                        deltaX = 0.1 * (cos (0) + cos (2 * pi / 6)) :: GLfloat
                        even = [ Hexagon (x - 0.5) (y - 0.5)   0.1 (0.0, 0.0, 1.0) | x <- [0,deltaX*2 .. 1], y <- [0,deltaY..1] ]
                        odd  = [ Hexagon (x - 0.5) (y - 0.5)   0.1 (0.0, 0.0, 1.0) | x <- [deltaX, deltaX*3 .. 1], y <- [deltaY*0.5,deltaY*1.5..1] ]

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window            <- createWindow "Hello World"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
  clear [ColorBuffer]
  forM_ generateLattice $ \hexagon ->
    preservingMatrix $ do
      color3f 1 1 1
      renderHex hexagon 
  flush
