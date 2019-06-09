{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

-- | Export only those functions useful for functional logic
module Lessons.Logic(
    (+),(-),(*),(/),(==), max
    , fromInteger, fromRational
    , drawingOf, lettering, printed
    ) where

import Prelude

