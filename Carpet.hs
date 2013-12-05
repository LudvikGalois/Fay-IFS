-- Simple Fay program for drawing a barnsley fern
-- values are hardcoded because I'm lazy and still
-- trying to work out how to use Fay
--
-- Robert 'Probie' Offner

import Prelude
import FFI

data IFS = IFS [(Probability, Transition)]
    deriving Show

data Transition = Affine Double Double Double Double Double Double
--                 | Möbius (Complex Double) (Complex Double) (Complex Double) (Complex Double)
    -- Fay doesn't support complex numbers
    deriving Show
    
type Probability = Double

buildIFS :: [(Probability, Transition)] -> IFS
buildIFS trans = IFS $ (\(x,y) -> zip (cdf x) y) $ unzip trans
    where cdf [] = []
          cdf (x:xs) = x : map (+ x) (cdf xs)

simulateIFS :: IFS -> (Double,Double) -> [Probability] -> [(Double,Double)]
simulateIFS _ _ [] = []
simulateIFS ifs xn (p:ps) = let xn' = applyTransition (chooseTransition p ifs) xn in
                            xn' : simulateIFS ifs xn' ps
          
carpet = buildIFS $
    [ (1/8, Affine (1/3) 0 0 (1/3) 0 0)
    , (1/8, Affine (1/3) 0 0 (1/3) 0 (1/3))
    , (1/8, Affine (1/3) 0 0 (1/3) 0 (2/3))
    , (1/8, Affine (1/3) 0 0 (1/3) (1/3) 0)
    , (1/8, Affine (1/3) 0 0 (1/3) (1/3) (2/3))
    , (1/8, Affine (1/3) 0 0 (1/3) (2/3) (1/3))
    , (1/8, Affine (1/3) 0 0 (1/3) (2/3) 0)
    , (1/8, Affine (1/3) 0 0 (1/3) (2/3) (2/3))
    ]

chooseTransition :: Probability -> IFS -> Transition
chooseTransition p (IFS x) = chooseTransition' p x

chooseTransition' _ [(_,x)] = x
chooseTransition' p ((p',x):xs) = if p <= p' then x else chooseTransition' p xs

applyTransition :: Transition -> (Double,Double) -> (Double,Double)
applyTransition (Affine a b c d e f) (x,y) = (a*x + b*y + e, c*x + d*y + f)

randDouble :: Fay Double
randDouble = ffi "Math.random()"
          
main = do
    x <- newRef 0
    y <- newRef 0
    setFillStyle "#FF0000" 
    setInterval 400 (drawStuff 2000 x y )

drawStuff :: Int -> Ref Double -> Ref Double -> Fay ()
drawStuff 0 x y = return ()
drawStuff n x y = do
    p <- randDouble
    xval <- readRef x
    yval <- readRef y
    let (x', y') = applyTransition (chooseTransition p carpet) (xval, yval)
    drawDot x' y' 
    writeRef x x'
    writeRef y y'
    drawStuff (n-1) x y 
    

drawDot :: Double -> Double -> Fay ()
drawDot = ffi "document.getElementById(\"fractal\").getContext(\"2d\").fillRect((%1)*500+150, 540 - %2*500, 1, 1)"
    
setFillStyle :: String -> Fay ()
setFillStyle = ffi "document.getElementById(\"fractal\").getContext(\"2d\").fillStyle=%1"

setInterval :: Int -> Fay () -> Fay ()
setInterval = ffi "setInterval(%2,%1)"

alert :: String -> Fay ()
alert = ffi "window.alert(%1)"

-- Refs
-- This will be provided in the fay package by default.

data Canvas
data Context

data Ref a
instance Show (Ref a)

newRef :: a -> Fay (Ref a)
newRef = ffi "new Fay$$Ref(%1)"

writeRef :: Ref a -> a -> Fay ()
writeRef = ffi "Fay$$writeRef(%1,%2)"

readRef :: Ref a -> Fay a
readRef = ffi "Fay$$readRef(%1)"
