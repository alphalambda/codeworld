{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ParallelListComp #-}

{-
  Copyright 2019 The CodeWorld Authors. All rights reserved.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-}
module Internal.Exports (
    -- * Entry points
      Program
    , drawingOf
    , animationOf
    , activityOf
    , debugActivityOf
    , groupActivityOf
    -- * Pictures
    , Picture
    , codeWorldLogo
    , circle
    , solidCircle
    , thickCircle
    , rectangle
    , solidRectangle
    , thickRectangle
    , pictures
    , (&)
    , coordinatePlane
    , blank
    , colored
    , coloured
    , translated
    , scaled
    , dilated
    , rotated
    , reflected
    , clipped
    , polyline
    , thickPolyline
    , polygon
    , thickPolygon
    , solidPolygon
    , curve
    , thickCurve
    , closedCurve
    , thickClosedCurve
    , solidClosedCurve
    , arc
    , sector
    , thickArc
    , lettering
    , styledLettering
    , Font(..)
    , TextStyle(..)
    -- * Colors
    , Color
    , Colour
    , pattern RGBA
    , pattern RGB
    , pattern HSL
{-
    , black
    , white
    , red
    , green
    , blue
    , yellow
    , orange
    , brown
    , pink
    , purple
    , gray
    , grey
-}
    , mixed
    , light
    , dark
    , bright
    , dull
    , translucent
    , assortedColors
    , lighter
    , darker
    , brighter
    , duller
    , hue
    , saturation
    , luminosity
    , alpha
    -- * Points and vectors
    , Point
    , translatedPoint
    , rotatedPoint
    , reflectedPoint
    , scaledPoint
    , dilatedPoint
    , Vector
    , vectorLength
    , vectorDirection
    , vectorSum
    , vectorDifference
    , scaledVector
    , rotatedVector
    , dotProduct
    , Picture
    , Font(..)
    , TextStyle(..)
    , blank
    , polyline
    , path
    , thickPolyline
    , thickPath
    , polygon
    , thickPolygon
    , solidPolygon
    , curve
    , thickCurve
    , closedCurve
    , thickClosedCurve
    , solidClosedCurve
    , rectangle
    , solidRectangle
    , thickRectangle
    , centerDot
    , dot
    , circle
    , solidCircle
    , thickCircle
    , arc
    , sector
    , thickArc
    , lettering
    , styledLettering
    , colored
    , coloured
    , translated
    , scaled
    , dilated
    , rotated
    -- , pictures
    , (&)
    , coordinatePlane
    , codeWorldLogo
    -- * Events
    , Event(..)
    -- * Debugging
    , traced
    -- Control Flow
    , composition
    , distribute
    , flat
    , xCoord
    , yCoord
    -- Pipes
    , (|>)
    , apply
    , clap
    , clone
    , map
    , filter
    ) where

import "base" Prelude (map,filter,IO)

import Internal.Num
import Internal.Prelude
import Internal.Text

import Internal.CodeWorld
import Internal.Color
import Internal.Event
import Internal.Picture hiding (coordinatePlane)

coordinatePlane :: Picture
coordinatePlane = polyline([(-10,0),(10,0)]) & polyline([(0,-10),(0,10)])

composition :: [Picture] -> Picture
composition = pictures

flattened :: [Picture] -> Picture
flattened = pictures

flat :: [Picture] -> Picture
flat = pictures

dot :: (Number,Number) -> Picture
dot(x,y) = translated(centerDot,x,y)

centerDot :: Picture
centerDot = solidCircle(0.1)

(|>) :: a -> (a -> b) -> b
x |> f = f(x)

apply :: ( ((input,param) -> output ), [param] ) -> [input] -> [output]
apply(f,xs)(os) = [ f(o,x) | x <- xs | o <- os ]

clone :: o -> [o]
clone(o) = repeating([o])

clap :: ( ((Picture,x) -> Picture), [x] ) -> Picture -> Picture
clap(f,xs)(o) = pictures([ f(o,x) | x <- xs ])

distribute :: ((a -> b), [a]) -> [b]
distribute(f,xs) = map f xs

xCoord :: Point -> Number
xCoord(x,_) = x

yCoord :: Point -> Number
yCoord(_,y) = y

