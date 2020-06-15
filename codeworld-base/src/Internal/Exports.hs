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
    -- * Operations on Pictures
    , right
    , up
    , colored
    , painted
    , scaled
    , dilated
    , rotated
    , clipped
    , translated
    , clap
    -- * Colors
    , Color
    , colorNamed
    , pattern RGBA
    , pattern RGB
    , pattern HSL
    , rgb
    , greyed
    , mixed
    , light
    , dark
    , bright
    , dull
    , translucent
    , withAlpha
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
    , upPoint
    , rightPoint
    , rotatedPoint
    , reflectedPoint
    , scaledPoint
    , dilatedPoint
    , translatedPoint
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
    , distributed
    , iterated
    , map
    , filter
    , fixed1
    , fixed2
    -- * Parameters
    , newParams
    , get
    , set
    -- * Events
    , Event(..)
    -- * Debugging
    , traced
    ) where

import "base" Prelude (IO)
import qualified "base" Prelude as P

import qualified Data.Map.Strict as M
import qualified Data.Text as T

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
-- > dot(0,0) |> clap(translated, [(-1,0),(0,0),(1,0) ])
--
-- All the cloned and transformed Pictures are 'combined' into
-- a single resulting Picture.
--
clap :: ( ((Picture,x) -> Picture), [x] ) -> Picture -> Picture
clap(f,xs)(o) = combined([ f(o,x) | x <- xs ])

-- | @dot(x,y)@ is a 'Picture' of a small dot centered at the 'Point' @(x,y)@.
dot :: Point -> Picture
dot(p) = translated(centerDot,p)

-- | A copy of the given @picture@ that is shown the given number of units
-- to the right of the original one.
right :: (Picture,Number) -> Picture
right(p,n) = translated(p,(n,0))

-- | A copy of the given @picture@ that is shown the given number of units
-- up from the original one.
up :: (Picture,Number) -> Picture
up(p,n) = translated(p,(0,n))

-- | A 'Point' that is the given number of units to the right of the
-- given 'Point'.
rightPoint :: (Point,Number) -> Point
rightPoint((x,y),n) = (x+n, y)

-- | A 'Point' that is the given number of units up from the
-- given 'Point'.
upPoint :: (Point,Number) -> Point
upPoint((x,y),n) = (x, y+n)

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
-- >                     |> fixed2(translated,(2,5))
-- >                     |> fixed2(rotated,45)
-- >                     |> combined)
--
-- The may need auxiliary functions, such as 'fixed1' and 'fixed2', to
-- specify which argument to pipe, as the example above illustrates.
(|>) :: a -> (a -> b) -> b
x |> f = f(x)

-- | This function is similar to 'distributed' but uses functions of two
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

-- | @distributed(transformation,objects)@ is a list of
-- objects, where each object is created by applying the given
-- 'transformation' to each object in the given list of 'objects'.
distributed :: ((a -> b), [a]) -> [b]
distributed(f,xs) = map f xs

-- | Select those elements from a list which satisfy the given predicate.
-- The list usually comes from a pipeline.
filter :: (a -> Truth) -> [a] -> [a]
filter = P.filter

-- | @iterated(transform,[parameter0,parameter1,...])@ is a list
-- where each element
-- is the result of applying the given @transform@ with a
-- @parameter@ to the previous element. The given parameter list is
-- consumed sequentially, so that each new application
-- uses a new @parameter@.
-- When an initial @object0@ comes from a pipeline, the result is
-- @[object0, object1, object2, ... ]@
-- where
-- @object1@ is @transform(object0, parameter0)@,
-- @object2@ is @transform(object1, parameter1)@
-- and so on.
iterated :: ((object,param) -> object,[param]) -> object -> [object]
iterated(f,[])(o) = [o]
iterated(f,p:ps)(o) = o : iterated(f,ps)(next)
    where
    next = f(o,p)

-- | Apply the given function to each element of a list that comes from
-- a pipeline.
map :: (a -> b) -> [a] -> [b]
map = P.map

-- | Partially apply a function of two arguments to a given first
-- argument, keeping it fixed while leaving the second one free.
--
fixed1 :: ((a,b) -> c, a) -> b -> c
fixed1(f,a)(b) = f(a,b)

-- | Partially apply a function of two arguments to a given second
-- argument, keeping it fixed while leaving the first one free.
--
fixed2 :: ((a,b) -> c, b) -> a -> c
fixed2(f,b)(a) = f(a,b)

type Params = M.Map T.Text Number

newParams :: Params
newParams = M.empty

set :: (Text,Number) -> Params -> Params
set(k,v) = M.insert (fromCWText k) v

get :: (Text,Number) -> Params -> Number
get(k,v) = M.findWithDefault v (fromCWText k)
