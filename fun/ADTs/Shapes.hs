module Shapes where

data Color = Red | Green | Blue
            deriving (Show, Eq)

-- type RGBColor = (Double, Double, Double) (1.0,1.0,0.0)
data RGBColor = RBGcolor Double Double Double

data Point3D = Point3D Double Double Double 

data Pair a b = Pair a b
                deriving(Show, Eq)

data Shape = Circle Double
           | Square Double
           | Rectangle Double Double
           deriving (Show, Eq)

area :: Shape ->  Double
area (Circle r)      = pi * (r ^ 2)
area (Square s)      = s ^ 2 
area (Rectangle w h) = w * h 

rot90 :: Shape -> Shape
rot90 (Rectangle w h) = Rectangle h w
rot90 x               = x

height :: Shape -> Double 
area (Circle r)      = r ^ 2
area (Square s)      = s
area (Rectangle w h) = h 

-- outleft
outl:: Pair a b -> a
outl (Pair x y) = x

-- outright
outr:: Pair a b -> b
outr (Pair x y) = y