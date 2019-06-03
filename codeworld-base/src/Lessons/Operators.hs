{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

-- | Alternative definitions of arithmentic operators as regular functions
module Lessons.Operators(
    add, sub, mul, div
    ) where

import Prelude

-- | add the two given numbers
add :: (Number,Number) -> Number
add(x,y) = x + y

-- | subtract the first number from the first one
sub :: (Number,Number) -> Number
sub(x,y) = x - y

-- | multiply the two given numbers
mul :: (Number,Number) -> Number
mul(x,y) = x * y

-- | divide the first number by the second one
div :: (Number,Number) -> Number
div(x,y) = x / y

-- | raise the first number to the power of the second one
pow :: (Number,Number) -> Number
pow(x,y) = x ^ y
