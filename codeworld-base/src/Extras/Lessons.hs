
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

module Extras.Lessons
where

import qualified "base" Prelude as P

import Prelude

import Extras.Op((<$>))

u3c1l1a2 :: [Number] -> [Number]
u3c1l1a2(random) = truncated <$> (230 +) <$> (538 *) <$> random

object =
    [ translated(solidCircle(0.05),-5.17, 4.21)
        & translated(thickRectangle(3.42,5.76,0.1),-5.17,4.21)
    ]

