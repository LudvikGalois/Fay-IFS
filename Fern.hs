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
          
cross = buildIFS $
    [ (0.1, Affine (1/2) 0 0 (1/2) (-0.5) (-0.5))
    , (0.1, Affine (1/2) 0 0 (1/2) (0.5) (0.5))
    , (0.1, Affine (1/2) 0 0 (1/2) (-0.5) (0.5))
    , (0.1, Affine (1/2) 0 0 (1/2) (0.5) (-0.5))
    , (0.6/4, Affine (1/2) 0 0 (1/2) (-1) (-1))
    , (0.6/4, Affine (1/2) 0 0 (1/2) (-1) (1))
    , (0.6/4, Affine (1/2) 0 0 (1/2) 1 (-1))
    , (0.6/4, Affine (1/2) 0 0 (1/2) 1 1)
    ]
                            
koch = buildIFS $
    [ (0.25, Affine (1/3) 0 0 (1/3) 0 0)
    , (0.25, Affine (1/3) 0 0 (1/3) (2/3) 0)
    , (0.25, Affine (1/6) (-(sqrt 3) / 6) ((sqrt 3) / 6) (1/6) (1/3) 0)
    , (0.25, Affine (1/6) ((sqrt 3) / 6) (-(sqrt 3) / 6) (1/6) (0.5) (sqrt 3 / 6))
    ]                        
    
barnsleyFern = buildIFS $
    [ (0.01, Affine 0 0 0 0.16 0 0)
    , (0.85, Affine 0.85 0.04 (-0.04) 0.85 0 1.6)
    , (0.07, Affine 0.2 (-0.26) 0.23 0.22 0 1.6)
    , (0.07, Affine (-0.15) 0.28 0.26 0.24 0 0.44)
    ]
          
dragon = buildIFS $
    [ (0.5, Affine a1 b1 c1 d1 0 0)
    , (0.5, Affine a2 b2 c2 d2 1 0)
    ]
  where a1 = (1 / sqrt 2) * cos (pi / 4)
        b1 = (1 / sqrt 2) * (negate (sin (pi / 4)))
        c1 = -1 * b1
        d1 = a1
        a2 = (1 / sqrt 2) * cos (0.75 * pi)
        b2 = (1 / sqrt 2) * (negate (sin (pi * 0.75)))
        c2 = -1 * b2
        d2 = a2

thingy = buildIFS $
    [ (0.5, Affine 0.2020 (-0.8050) (-0.6890) (-0.3420) (-0.3730) (-0.6530))
    , (0.5, Affine 0.1380 0.6650 (-0.5020) (-0.2220) 0.6600 (-0.2770))
    ]
        
sierpinksi = buildIFS $
    [ (1/3, Affine 0.5 0 0 0.5 0 0)
    , (1/3, Affine 0.5 0 0 0.5 0.25 ((sqrt 3) / 4))
    , (1/3, Affine 0.5 0 0 0.5 0.5 0)
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
    setFillStyle "#00FF00" 
    setInterval 400 (drawStuff 1000 x y )

drawStuff :: Int -> Ref Double -> Ref Double -> Fay ()
drawStuff 0 x y = return ()
drawStuff n x y = do
    p <- randDouble
    xval <- readRef x
    yval <- readRef y
    let (x', y') = applyTransition (chooseTransition p barnsleyFern) (xval, yval)
    drawDot x' y' 
    writeRef x x'
    writeRef y y'
    drawStuff (n-1) x y 
    

drawDot :: Double -> Double -> Fay ()
drawDot = ffi "document.getElementById(\"fractal\").getContext(\"2d\").fillRect((%1+3)*100, 600 - %2*60, 1, 1)"
    
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
