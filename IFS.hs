-- Simple Fay program for drawing an IFS 
-- limited to 10 transforms for ease of implementing
-- trying to work out how to use Fay
-- but fairly sure I'm doing it wrong
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
          
chooseTransition :: Probability -> IFS -> Transition
chooseTransition p (IFS x) = chooseTransition' p x

chooseTransition' _ [(_,x)] = x
chooseTransition' p ((p',x):xs) = if p <= p' then x else chooseTransition' p xs

applyTransition :: Transition -> (Double,Double) -> (Double,Double)
applyTransition (Affine a b c d e f) (x,y) = (a*x + b*y + e, c*x + d*y + f)

randDouble :: Fay Double
randDouble = ffi "Math.random()"
          
data Window = Window Double Double Double Double

main = do
    x <- newRef 0
    y <- newRef 0
    win <- newRef (Window 0 1 0 1)
    ifs <- newRef $ buildIFS [(1, Affine 0 0 0 0 0 0)]
    time <- newRef 100
    dots <- newRef 100
    timer <- (setInterval 100000 (return ())) >>= newRef
    addWindowEvent "load" $ do
        setHeadHtml "<style> canvas{float:left; margin:10px;} </style>"
        setBodyHtml "<h1> IFS drawing program </h1>"
        addBodyHtml "<h3> with at most 10 functions (which must be affine transforms), because adding rows to tables is effort</h3>"
        addBodyHtml "<p> Functions are of the form f(x,y) = (a*x+b*y+e, c*x+d*y+f) and chosen with probability p </p>"
        addBodyHtml "<canvas id =\"fractal\" width=\"800\" height=\"600\" style=\"border:1px solid #000000;\">"
        addBodyHtml $ concat
            [ "<table border=\"1\">" 
            , tr $ concatMap (\s -> "<th>" ++ s ++ "</th>") ["Setting", "Value"]
            , tr $ concat [td "Time (in ms) between updates"
                          ,td "<input type=\"number\" id=\"speed\" value=\"0\" size=\"7\">"]
            , tr $ concat [td "Dots per update", td "<input type=\"number\" id=\"dotcount\" value=\"0\" size=\"7\">"]
            , tr $ concat [td "Minimum expected X", td "<input type=\"number\" id=\"xmin\" value=\"0\" size=\"7\">"]
            , tr $ concat [td "Maximum expected X", td "<input type=\"number\" id=\"xmax\" value=\"1\" size=\"7\">"]
            , tr $ concat [td "Minimum expected Y", td "<input type=\"number\" id=\"ymin\" value=\"0\" size=\"7\">"]
            , tr $ concat [td "Maximum expected Y", td " <input type=\"number\" id=\"ymax\" value=\"1\" size=\"7\">"]
            , tr $ concat [td "Background Colour", td " <input type=\"text\" id=\"bg\" value=\"#000000\" size=\"7\">"]
            , tr $ concat [td "Foreground Colour", td " <input type=\"text\" id=\"fg\" value=\"#FFFFFF\" size=\"7\">"]
            , "</table>"
            ]
        addBodyHtml $ concat 
            [ "<table border=\"1\">" 
            , "<tr>"
            , concatMap (\c -> "<th>" ++ [c] ++ " </th>") " abcdefp"
            , "</tr>"
            , concatMap addFunction [1..10] 
            ,"</table>"
            ]
        addBodyHtml "<button id=\"redraw\"> Redraw </button>"
        startupSierpinski
        setupButton (redraw x y ifs time dots timer win)
        redraw x y ifs time dots timer win

startupSierpinski = do
    setBox "speed" "500"
    setBox "dotcount" "1000"
    setBox "xmin" "-0.5"
    setBox "xmax" "1.5"
    setBox "ymin" "-1"
    setBox "ymax" "1"
    setBox "p1" "0.5"
    setBox "a1" "0.5"
    setBox "b1" "-0.5"
    setBox "c1" "0.5"
    setBox "d1" "0.5"
    setBox "p2" "0.5"
    setBox "a2" "-0.5"
    setBox "b2" "0.5"
    setBox "c2" "-0.5"
    setBox "d2" "-0.5"
    setBox "e2" "1"

redraw x y ifs time dots timer win = do
    writeRef x 0
    writeRef y 0
    readRef timer >>= clearInterval 
    getBox "bg" >>= setFillStyle
    ffi "document.getElementById(\"fractal\").getContext(\"2d\").fillRect(1, 1, 800, 600)" :: Fay ()
    getBox "fg" >>= setFillStyle
    getIntBox "speed" >>= writeRef time
    getIntBox "dotcount" >>= writeRef dots
    xmin <- getFloatBox "xmin"
    xmax <- getFloatBox "xmax"
    ymin <- getFloatBox "ymin"
    ymax <- getFloatBox "ymax"
    funs <- sequence $ map readF [1..10]
    writeRef win (Window xmin xmax ymin ymax)
    writeRef ifs (buildIFS funs)
    drawLoop x y ifs time dots timer win

readF :: Int -> Fay (Probability, Transition)
readF n = do
    p <- getFloatBox ('p':show n)
    a <- getFloatBox ('a':show n)
    b <- getFloatBox ('b':show n)
    c <- getFloatBox ('c':show n)
    d <- getFloatBox ('d':show n)
    e <- getFloatBox ('e':show n)
    f <- getFloatBox ('f':show n)
    return (p, (Affine a b c d e f))

addFunction :: Int -> String
addFunction n = tr $ concat $
    [ td $ "<i> f" ++ show n ++ " </i>"
    , (inputBox 'a' n)
    , (inputBox 'b' n)
    , (inputBox 'c' n)
    , (inputBox 'd' n)
    , (inputBox 'e' n)
    , (inputBox 'f' n)
    , (inputBox 'p' n)
    ]
        
tr x = "<tr>" ++ x ++ "</tr>"
td x = "<td>" ++ x ++ "</td>"

setBox :: String -> String -> Fay ()
setBox = ffi "document.getElementById(%1).value=%2"

inputBox :: Char -> Int -> String
inputBox c n = td $ "<input type=\"number\" id=\"" ++ val ++ "\" value = \"0\" size=\"4\">"
    where val = c:(show n)

getFloatBox :: String -> Fay Double
getFloatBox = ffi "parseFloat(document.getElementById(%1).value)"

getIntBox :: String -> Fay Int
getIntBox = ffi "parseInt(document.getElementById(%1).value)"

getBox :: String -> Fay String
getBox = ffi "document.getElementById(%1).value"

drawLoop :: Ref Double -> Ref Double -> Ref IFS -> Ref Int -> Ref Int -> Ref Timer -> Ref Window -> Fay ()
drawLoop x y ifs time dots timer win = do
    time' <- readRef time
    dots' <- readRef dots
    ifs' <- readRef ifs
    win' <- readRef win
    (setInterval time' (drawStuff dots' x y ifs' win')) >>= writeRef timer 
    

drawStuff :: Int -> Ref Double -> Ref Double -> IFS -> Window -> Fay ()
drawStuff 0 _ _ _  _ = return ()
drawStuff n x y ifs win@(Window xmin xmax ymin ymax) = do
    p <- randDouble
    xval <- readRef x
    yval <- readRef y
    let (x', y') = applyTransition (chooseTransition p ifs) (xval, yval)
    drawDot  (warp x' xmin xmax 800) (warp y' ymin ymax 600) 
    writeRef x x'
    writeRef y y'
    drawStuff (n-1) x y ifs win

warp point minpoint maxpoint scaling = (point - minpoint) * (scaling / (maxpoint - minpoint))
    
setHeadHtml :: String -> Fay ()
setHeadHtml = ffi "document.head.innerHTML = %1"

setBodyHtml :: String -> Fay ()
setBodyHtml = ffi "document.body.innerHTML = %1"

addBodyHtml :: String -> Fay ()
addBodyHtml = ffi "document.body.innerHTML += %1"

addWindowEvent :: String -> Fay () -> Fay ()
addWindowEvent = ffi "window.addEventListener(%1, %2)"

setupButton :: Fay () -> Fay ()
setupButton = ffi "document.getElementById(\"redraw\").onclick=%1"

drawDot :: Double -> Double -> Fay ()
drawDot = ffi "document.getElementById(\"fractal\").getContext(\"2d\").fillRect(%1, 600 - %2, 1, 1)"
    
setFillStyle :: String -> Fay ()
setFillStyle = ffi "document.getElementById(\"fractal\").getContext(\"2d\").fillStyle=%1"

setInterval :: Int -> Fay () -> Fay Timer
setInterval = ffi "setInterval(%2,%1)"

clearInterval :: Timer -> Fay ()
clearInterval = ffi "clearInterval(%1)"

alert :: String -> Fay ()
alert = ffi "window.alert(%1)"

-- Refs
-- This will be provided in the fay package by default.

data Timer

data Ref a
instance Show (Ref a)

newRef :: a -> Fay (Ref a)
newRef = ffi "new Fay$$Ref(%1)"

writeRef :: Ref a -> a -> Fay ()
writeRef = ffi "Fay$$writeRef(%1,%2)"

readRef :: Ref a -> Fay a
readRef = ffi "Fay$$readRef(%1)"
