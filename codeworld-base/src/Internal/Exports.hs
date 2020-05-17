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
    -- * Pictures and Basic Shapes
    , Picture
    , combined
    , blank
    , centerDot
    , dot
    , circle
    , solidCircle
    , thickCircle
    , rectangle
    , solidRectangle
    , thickRectangle
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
    , clap
    -- * Operations on Pictures
    , (&)
    , colored
    , translated
    , scaled
    , dilated
    , rotated
    , clipped
    -- * Colors
    , Color
    , pattern RGBA
    , pattern RGB
    , pattern HSL
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
    -- * Points
    , Point
    , xCoord
    , yCoord
    , translatedPoint
    , rotatedPoint
    , reflectedPoint
    , scaledPoint
    , dilatedPoint
    -- * Vectors
    , Vector
    , vectorLength
    , vectorDirection
    , vectorSum
    , vectorDifference
    , scaledVector
    , rotatedVector
    , dotProduct
    -- * Pipes and Control Flow
    , (|>)
    , apply
    , clone
    , distribute
    , map
    , filter
    , partial
    -- * Events
    , Event(..)
    -- * Debugging
    , traced
    ) where

import "base" Prelude (map,filter,IO)

import Internal.Num
import Internal.Prelude
import Internal.Text

import Internal.CodeWorld
import Internal.Color
import Internal.Event
import Internal.Picture hiding (coordinatePlane)

-- | The name of this function is a portmanteau of 'clone' and 'apply'.
-- This function is intended to be used with pipes.
-- A typical use has the form
-- 
-- > picture |> clap(transform,parameters)
-- 
-- where 'transform' is a function that takes a Picture and a parameter.
-- The input 'picture' coming from the pipeline is cloned, and the given
-- 'transform' is applied to each clone, each time with a different parameter
-- from the list of given 'parameters'.
--
-- For example, the following code will produce several dots along the X axis:
--
-- > centerDot |> clap(translated, [(-1,0),(0,0),(1,0) ])
--
-- All the cloned and transformed Pictures are 'combined' into
-- a single resulting Picture.
--
clap :: ( ((Picture,x) -> Picture), [x] ) -> Picture -> Picture
clap(f,xs)(o) = combined([ f(o,x) | x <- xs ])

-- | 'dot(x,y)' is a Picture of a small dot centered at the Point (x,y).
dot :: (Number,Number) -> Picture
dot(x,y) = translated(centerDot,x,y)

-- | A Picture of a dot standing at the origin.
centerDot :: Picture
centerDot = solidCircle(0.1)

--------------------------------------------------------------------------------
-- Pipes and Control Flow
--------------------------------------------------------------------------------

-- | The `pipe` operator is an alternative form of specifying
-- function application.
-- It facilitates the writing of expressions in which the data flows
-- from left to right,
-- instead of right to left. It can also be used to convert a nested expression
-- to a flat `pipeline`.
-- 
-- The pipe operator is defined with the following equation:
-- 
-- > x |> f = f(x)
-- 
-- For example, the following program:
--
-- > program = drawingOf(rotated(translated(rectangle(1,3),(2,5)),45))
--
-- can be rewritten using the pipe as:
-- 
-- > program = drawingOf(rectangle(1,3) 
-- >                     |> clap(translated,[(2,5)]) 
-- >                     |> clap(rotated, [45])
-- >                     |> combined)
--
-- The pipe is especially useful when combined with the 'clap'
-- function, as the example above illustrates.
(|>) :: a -> (a -> b) -> b
x |> f = f(x)

-- | This function is similar to 'distribute' but uses functions of two
-- arguments, where the first argument is an `object` and the second argument
-- is a `parameter`. When a list of `objects` is received from a pipeline,
-- an output stream of transformed objects is produced, where a different
-- parameter is used for each object transformation.
--
-- For example, the following code will produce a list of shapes of
-- different colors:
--
-- > [solidCircle(1),solidRectangle(2,3),solidPolygon([(-1,0),(0,1),(1,0)])
-- > |> apply(colored, [RGB(1,0,0), RGB(0,1,0), RGB(0,0,1)])
--
apply :: ( ((input,param) -> output ), [param] ) -> [input] -> [output]
apply(f,xs)(os) = [ f(o,x) | x <- xs | o <- os ]

-- | An infinite list of clones of the given 'object'
clone :: object -> [object]
clone(o) = repeating([o])

-- | @distribute(transformation,objects)@ applies the given 'transformation'
-- to each object in the given list of 'objects'.
distribute :: ((a -> b), [a]) -> [b]
distribute(f,xs) = map f xs

-- | Partially apply a function of two arguments to a given first
-- argument, leaving the second one free to be received via a pipeline.
--
-- For example, the following code is equivalent to @rotated(centerDot,45)@
--
-- > 45 |> partial(rotated,centerDot)
--
-- This function is intended to be used in pipelines.
--
partial :: ( (a,b) -> c, a ) -> b -> c
partial(f,a)(b) = f(a,b)
