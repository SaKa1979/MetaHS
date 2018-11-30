module Example
    where

type CarType = (String, String)

data Color = Red | Green | Blue deriving (Show)

data Car = Car
    { carType :: CarType
    , color   :: Color
    , age     :: Int
    } deriving (Show)

newCar :: String -> String -> Color -> Car
newCar brandName typeName newColor = Car
    { carType = (brandName, typeName)
    , color = newColor
    , age = 0
    }

repaint :: Car -> Color -> Car
repaint car newColor = car { color = newColor }

magicNumber :: Int
magicNumber = 42

isMagic :: Int -> Bool
isMagic = (== magicNumber)
