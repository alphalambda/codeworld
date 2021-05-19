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
    -- * Vectors
    , Vector
    , vectorLength
    , vectorDirection
    , vectorSum
    , vectorDifference
    , scaledVector
    , rotatedVector
    , dotProduct
    -- * Animations
    , Animation
    , staticMotion, repeatedMotion, delayedMotion, fasterMotion
    , transformedMotion, combinedMotions, showBetween, travelBetween
    , saw
    -- * Control Flow
    , cloned
    , distributed
    , iterated
    , on
    -- * Parameters
    , newParams
    , get
    , set
    -- * Events
    , Event(..)
    -- * Debugging
    , traced
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
    , run, repeatFor, repeatWhile
    , foreach, forloop, whileloop
    -- * List manipulation
    , prepend, append, list, listn, butLast, pairs, unpairs, zipped, unzipped
    , indexOf
    -- * Text formatting
    , lJustified, rJustified
    , printedDecimals, printedNumbers, printedPoint
    , letteringBlock, letteringBlockLengths
    -- * Other useful functions
    , cumulativeSums
    , pass
    , textHash
    -- * Numbers
    , module Internal.Num
    -- * Text
    , module Internal.Text
    -- * General purpose functions
    , module Internal.Prelude
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
import Internal.Picture
import Internal.Util

{-

import "base" Prelude (IO)

import Internal.Num
import Internal.Prelude hiding (randomsFrom)
import Internal.Text hiding (fromCWText, toCWText)

import Internal.Util
{-
import Internal.CodeWorld
import Internal.Color
import Internal.Event
import Internal.Picture
-}

-}

