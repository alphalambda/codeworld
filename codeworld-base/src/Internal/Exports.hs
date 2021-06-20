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
    , letteringBlock
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
    -- * Properties of Pictures
    , pictureBounds
    , outputBounds
    -- * Points
    , Point
    , xCoord
    , yCoord
    , upPoint
    , rightPoint
    , rotatedPoint
    , scaledPoint
    , dilatedPoint
    , translatedPoint
    -- * Polygon properties
    , polygonPerimeter
    , polygonArea
    , polygonCenter
    , polygonEdges
    , polylineLength
    , polylineEdges
    , polygonCorners
    , cornerAngles
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
    -- * Animations
    , Animation
    , staticMotion, repeatedMotion, delayedMotion, fasterMotion
    , transformedMotion, combinedMotions, showBetween, travelBetween
    , saw
    -- * Events
    , Event(..)
    -- * Predicates
    , Predicate, precedes, is_in, nonEmpty
    , between, above, below, exactly
    , all_, any_, excluded
    , selected, selectedValues, selectedKeys
    , choice, choices
    -- * Grouping and Sorting lists
    , logicalGroupBy, alphabeticalGroupBy, numericalGroupBy
    , alphabeticalSortedOn, numericalSortedOn
    -- * Control flow
    , distributed
    , iterated
    , foreach, forloop, whileloop
    , run, repeatFor, repeatWhile
    , on
    -- * Numbers
    , module Internal.Num
    , cumulativeSums
    -- * Text
    , module Internal.Text
    , lJustified, rJustified
    , printedDecimals, printedNumbers, printedPoint
    , textHash
    -- * General purpose functions
    , module Internal.Prelude
    , cloned
    , pass
    , prepend, append, list, listn, butLast, pairs, unpairs, zipped, unzipped
    , indexOf
    -- * Parameters
    , newParams
    , get
    , set
    -- * Vectors
    , Vector
    , vectorLength
    , vectorDirection
    , vectorSum
    , vectorDifference
    , scaledVector
    , rotatedVector
    , dotProduct
    -- * Debugging
    , traced
    ) where

import "base" Prelude (IO)
import qualified "base" Prelude as P

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Internal.Prelude hiding (randomsFrom)
import Internal.Num
import Internal.Text hiding (fromCWText, toCWText)

import Internal.CodeWorld
import Internal.Color
import Internal.Event
import Internal.Picture
import Internal.Util
