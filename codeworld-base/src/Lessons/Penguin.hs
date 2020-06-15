{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

-- | Pictures of a penguin, a bird, and a landscape
module Lessons.Penguin(
    bird, penguin, landscape, water
    ) where

import Prelude
import Extras.Colors

bird = dilated(figure,0.5)
  where
  figure =
    combined([ colored(tail, red)
             , translated(eye, (2.1, 1.1))
             , colored(body, red) 
             , colored(mouth, yellow)
             , translated(leg, (1.05, -1.5))
             , translated(leg, (1.95, -1.5))
             ])
  tail = solidPolygon([(0, 0), (-3, 3), (0, 4)])
  body = solidPolygon([(0, 0), (1, -1), (2, -1), (3, 0), (3, 2), (2, 2)])
  mouth = solidPolygon([(3, 0), (4, 1), (3, 2)])
  leg = solidRectangle(0.1, 1)
  eye = solidRectangle(0.2, 0.2)
  red = RGB(1,0,0)
  yellow = RGB(1,1,0)

penguin = translated(dilated(figure,0.5),(0,2.5))
  where
  figure = combined(
         [ colored(solidPolygon[(0,0),(-0.7,4),(0.7,4)],greyed(0.6))
         , colored(solidPolygon[(-0.7,4),(0.7,4),(0,6.5)],greyed(0.8))
         , colored(solidPolygon[(0,6.5),(0.7,4),(2,7)],greyed(0.8))
         , colored(solidPolygon[(0,6.5),(-0.7,4),(-2,7)],greyed(0.8))
         , solidPolygon[(-2,7),(-1.6,8.8),(-1.6,6.8)]
         , solidPolygon[(2,7),(1.6,8.8),(1.6,6.8)]
         , solidPolygon[(-1.6,8.8),(-0.8,9.5),(0,8.6)]
         , solidPolygon[(1.6,8.8),(0.8,9.5),(0,8.6)]
         , solidPolygon[(0,8.6),(-0.8,9.5),(0.8,9.5)]
         , colored(solidPolygon[(2,7),(0.7,4),(3,3)],greyed(0.7))
         , colored(solidPolygon[(-2,7),(-0.7,4),(-3,3)],greyed(0.7))
         , colored(solidPolygon[(-3,3),(-0.7,4),(0,0)],greyed(0.6))
         , colored(solidPolygon[(3,3),(0.7,4),(0,0)],greyed(0.6))
         , colored(solidPolygon[(0,0),(-3,3),(-2,-4)],greyed(0.6))
         , colored(solidPolygon[(0,0),(3,3),(2,-4)],greyed(0.6))
         , colored(solidPolygon[(0,0),(-2,-4),(0,-5)],greyed(0.65))
         , colored(solidPolygon[(0,0),(2,-4),(0,-5)],greyed(0.65))
         , solidPolygon[(0,-5),(-2,-4),(-2.5,-5)]
         , solidPolygon[(0,-5),(2,-4),(2.5,-5)]
         , colored(solidPolygon[(-3,3),(-2.7,-4.2),(-1.8,-4.6)],greyed(0.55))
         , colored(solidPolygon[(3,3),(2.7,-4.2),(1.8,-4.6)],greyed(0.55))
         , colored(solidPolygon[(-2.7,-4.2),(-3,3),(-4.6,-0.5)],greyed(0.7))
         , colored(solidPolygon[(2.7,-4.2),(3,3),(4.6,-0.5)],greyed(0.7))
         , colored(solidPolygon[(-3,3),(-4.6,-0.5),(-5,-0.6)],greyed(0.8))
         , colored(solidPolygon[(3,3),(4.6,-0.5),(5,-0.6)],greyed(0.8)) 
         , solidPolygon[(-2,7),(-5,1.5),(-5,-0.6),(-3,3)]
         , solidPolygon[(2,7),(5,1.5),(5,-0.6),(3,3)]
         , solidPolygon[(-1.3,8.8),(0,8.6),(-0.5,7.6)]
         , solidPolygon[(1.3,8.8),(0,8.6),(0.5,7.6)]
         , solidPolygon[(0,8.6),(-0.5,7.6),(0.5,7.6)]
         , colored(solidPolygon[(0,7.7),(-0.5,7.4),(0,7)],orange)
         , colored(solidPolygon[(0,7.7),(0.5,7.4),(0,7)],dark(orange))
         , solidPolygon[(-0.5,7.6),(-0.7,7.2),(0,7),(0.7,7.2),(0.5,7.6)]
         , solidPolygon[(-1.4,8),(-0.9,8),(-1,7.7)]
         , solidPolygon[(1.4,8),(0.9,8),(1,7.7)]
         , translated(colored(solidRectangle(3.2,2.5),white),(0,7.7))
         ])
  orange = colorNamed("orange")
  white = RGB(1,1,1)

water = painted(solidPolygon(pts), "aqua")
  where
  pts = [ (3,-1), (-10,-1), (-10,-10), (0,-10) ]
  
landscape = colored(solidPolygon(pts), greyed(0.9))
  where
  pts = [ (10,-10), (10,-1), (3,-1), (2,-4), (-1,-7), (-3,-10) ]
