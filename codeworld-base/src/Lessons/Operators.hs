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

add :: (Number,Number) -> Number
add(x,y) = x + y

sub :: (Number,Number) -> Number
sub(x,y) = x - y

mul :: (Number,Number) -> Number
mul(x,y) = x * y

div :: (Number,Number) -> Number
div(x,y) = x / y
