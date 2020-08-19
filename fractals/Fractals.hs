module Fractals where

  -- needed to display the picture in the playground
import Codec.Picture

  -- our line graphics programming interface
import LineGraphics

house :: Path
house = [(300, 750), (300, 450), (270, 450), (500, 200),
         (730, 450), (700, 450), (700, 750)]

door :: Path
door = [(420, 750), (420, 550), (580, 550), (580, 750)]

drawPng :: String -> Float -> Picture -> IO()
drawPng name line picture=  writePng name (drawPicture line picture)