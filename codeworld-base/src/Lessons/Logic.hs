{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

-- | Export only those functions useful for functional logic
module Lessons.Logic(
    (+),(-),(*),(/),(==), max, negate
    , fromInteger, fromRational, fromString
    , drawingOf, lettering, printed
    , solidCircle, solidRectangle, solidPolygon, up
    , translated, rotated, dilated, scaled, colored
    , combined, blank, joined
    , error
    , Number
    ) where

import Prelude

