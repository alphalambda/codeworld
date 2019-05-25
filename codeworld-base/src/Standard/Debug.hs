{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}

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
--------------------------------------------------------------------------------
-- | The standard set of functions and variables available to all programs.
-- This version is for debugging purposes only. Look at the documentation
-- in the "Standard" module for more information.
module Standard.Debug (
      module Include
    , drawingOf
    -- * Redefined drawing primitives
    , solidRectangle, solidCircle, solidPolygon, solidClosedCurve
    , sector
    ) where

import Prelude as Include hiding (
    drawingOf, solidRectangle, solidCircle, solidPolygon, solidClosedCurve
    , sector
    )

import qualified "codeworld-api" CodeWorld as CWA
import qualified Internal.CodeWorld as CWI
import qualified Internal.Picture as P

------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- LSU Modifications
------------------------------------------------------------------------------

solidRectangle = P.rectangle

solidCircle = P.circle

solidPolygon = P.polygon

solidClosedCurve = P.closedCurve

sector = P.arc

-- | Show a coordinate plane along with the given Picture. This is intended
-- for debugging purposes, so that you can place your objects more accurately.
-- However, it should not be used for your final product. As a reminder,
-- your picture is shown all in black.
drawingOf :: Picture -> Program
drawingOf pic = CWI.drawingOf(P.CWPic CWA.coordinatePlane & colored(pic,black))
