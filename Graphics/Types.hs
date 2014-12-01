module Graphics.Types where

import Graphics.Gloss (Picture)

class Drawable a where 
    draw :: a -> Float -> Float -> Picture
