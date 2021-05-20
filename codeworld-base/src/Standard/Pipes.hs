{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
-- | A set of functions and variables that provide additional support for your
-- programs.
--
-- To use a function defined in this module, you must begin your code with this
-- line:
--
-- > import Standard.Pipes(function)
--
-- where instead of @function@ you write the actual name of the function you
-- want to use.
--
-- You can specifiy more than one function. For example, if you want to use 3
-- functions, then instead of writing 3 separate @import@ lines, you can write
-- this:
--
-- > import Standard.Pipes(function1,function2,function3)
--

module Standard.Pipes(
    (|>)
    , apply
    , clap
    , pairedBefore
    , pairedAfter
    , map
    , filter
    , fixed1
    , fixed2
    ) where

import Internal.Prelude

import Internal.Num
import Internal.Picture
import Internal.Text

import qualified "base" Prelude as P

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

-- | @apply(transformations)@ takes inputs from a pipe and applies
-- the first transformation to the first input, the second transformation
-- to the second input, and so on.
--
-- For example, the following code will produce a list of shapes of
-- different colors:
--
-- > [solidCircle(1),solidRectangle(2,3),solidPolygon([(-5,0),(0,5),(5,0)])]
-- > |> pairedBefore([RGB(1,0,0), RGB(0,1,0), RGB(0,0,1)])
-- > |> zipped
-- > |> apply(cloned(colored))
--
apply :: [input -> output] -> [input] -> [output]
apply [] _ = []
apply _ [] = []
apply (f:fs) (v:vs) = f(v) : apply fs vs

-- | The name of this function is a portmanteau of 'cloned' and 'apply'.
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

-- | @pairedBefore(b)@ is a function that takes an input @a@ from
-- a pipe and produces the pair @(a,b)@
pairedBefore :: b -> a -> (a,b)
pairedBefore(b)(a) = (a,b)

-- | @pairedAfter(a)@ is a function that takes an input @b@ from
-- a pipe and produces the pair @(a,b)@
pairedAfter :: a -> b -> (a,b)
pairedAfter(a)(b) = (a,b)

-- | Select those elements from a list which satisfy the given predicate.
-- The list usually comes from a pipeline.
filter :: (a -> Truth) -> [a] -> [a]
filter = P.filter

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

