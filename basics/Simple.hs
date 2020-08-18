module Simple
where

inc :: Int -> Int
inc x = x + 1

avarage :: Float -> Float -> Float
avarage a b = (a + b) / 2

showResult :: Int -> String
showResult a = "The result is " ++ show a

showAreaOfCicke :: (Show a, Floating a) => a -> String
showAreaOfCicke r = "The area of a circle with radius " ++ show r ++ "cm is about " ++ show (r * r * pi) ++ " cm^2"

type ColorPoint = (Int, Int, String)

move :: ColorPoint -> Int -> Int -> ColorPoint
move (x, y, color) xDistance yDistance 
  = (x + xDistance, y + yDistance, color)

distance (x1, y1, color1) (x2, y2, color2) 
   = sqrt (fromIntegral(dx * dx + dy * dy))
  where
    dx = x1 - x2
    dy = y1 - y2

sort2 :: Int -> Int -> (Int, Int)
sort2 a b | a <= b = (a, b)
  | otherwise = (b, a)

isLower :: Char -> Bool
isLower c = 'a' <= c && c <= 'z'

mangle :: String -> String
mangle "" = ""
mangle s = tail s ++ [head s]

divide :: Int -> Int -> Int
divide x y = length (multiples y x)
  where
    multiples x max = [x, x + x..max]