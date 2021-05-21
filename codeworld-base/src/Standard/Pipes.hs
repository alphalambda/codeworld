{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
-- | A set of functions that provide an alternative way to organize your
-- programs.
--
-- This module needs to be imported as a whole, by adding the following
-- line at the beginning of your code:
--
-- > import Standard.Pipes
--

module Standard.Pipes(
    -- * General Pipes
    (|>)
    , apply
    , addFirst
    , addSecond
    , clap
    , map
    , filter
    , fixed1
    , fixed2
    , unzip
    , zip
    , zipFirst
    , zipSecond
    -- * Transformers
    , Transformer
    , colorIn
    , dilate
    , paintIn
    , rightBy
    , rotate
    , scale
    , translate
    , upBy
    ) where

import Internal.Prelude

import Internal.Num
import Internal.Picture
import Internal.Text
import Internal.Color

import qualified Data.List as L
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
-- > program = rectangle(1,3) 
-- >           |> translate(2,5)
-- >           |> rotate(45)
-- >           |> drawingOf
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
-- > |> zipFirst([RGB(1,0,0), RGB(0,1,0), RGB(0,0,1)])
-- > |> clap(colorIn)
--
apply :: [input -> output] -> [input] -> [output]
apply [] _ = []
apply _ [] = []
apply (f:fs) (v:vs) = f(v) : apply fs vs

-- | The name of this function is a portmanteau of clone and apply.
-- This function is intended to be used with pipes.
-- A typical use has the form
-- 
-- > (parameters,objects) |> clap(transform)
-- 
-- where 'transform' is a 'Picture' 'Transformer' that takes a parameter.
-- The 'transform' will be applied to a parameter and an object coming
-- from the corresponding lists. The 'parameters' and the 'objects' will
-- be consumed in order until one of them is exhausted.
--
-- For example, the following code will produce several dots along the X axis:
--
-- > dot(0,0) |> cloned |> addFirst[(-1,0),(0,0),(1,0)] |> clap(translate)
--
-- An equivalent way to do the same would be:
--
-- > ([(-1,0),(0,0),(1,0)], cloned(dot(0,0))) |> clap(translate)
--
-- All the cloned and transformed Pictures are 'combined' into
-- a single resulting Picture.
--
-- Even though the actual signature of this function is very general, the
-- following signature would be enough for typical uses:
--
-- > clap :: (param -> Transformer Picture) -> ([param],[Picture]) -> [Picture]
--
clap :: (a -> b -> c) -> ([a],[b]) -> [c]
clap(f)(as,bs) = [ f a b | a <- as | b <- bs ]

-- | @addFirst(a)@ is a function that takes an input @b@ from
-- a pipe and produces the pair @(a,b)@
addFirst :: a -> b -> (a,b)
addFirst(a)(b) = (a,b)

-- | @addSecond(b)@ is a function that takes an input @a@ from
-- a pipe and produces the pair @(a,b)@
addSecond :: b -> a -> (a,b)
addSecond(b)(a) = (a,b)

-- | @zipFirst(a)@ is a function that takes a list of inputs @b@ from
-- a pipe and produces a list of pairs by pairing one element
-- of the list @a@ with one element of the list @b@.
zipFirst :: [a] -> [b] -> [(a,b)]
zipFirst(as)(bs) = [ (a,b) | a <- as | b <- bs ]

-- | @zipSecond(b)@ is a function that takes a list of inputs @a@ from
-- a pipe and produces a list of pairs by pairing one element
-- of the list @a@ with one element of the list @b@.
zipSecond :: [b] -> [a] -> [(a,b)]
zipSecond(bs)(as) = [ (a,b) | a <- as | b <- bs ]

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

-- | Convert a list of pairs to a pair of lists
unzip :: [(a,b)] -> ([a],[b])
unzip = L.unzip

-- | Convert a pair of lists to a list of pairs
zip :: ([a],[b]) -> [(a,b)]
zip(a,b) = L.zip a b

-- Transformers

{-
    -- * Transformers
    , Transformer
    , colorIn
    , dilate
    , paintIn
    , rightBy
    , rotate
    , scale
    , translate
    , upBy
-}

-- | A @Transformer@ is a function that takes an object from a pipeline,
-- changes it, and returns the changed object to the pipeline to that
-- it can be further transformed.
type Transformer object = object -> object

-- | A 'Transformer' that colors a 'Picture' in the given color.
colorIn :: Color -> Transformer Picture
colorIn c p = colored(p,c)

-- | A 'Transformer' that dilates a 'Picture' by the given factor.
dilate :: Number -> Transformer Picture
dilate s p = dilated(p,s)

-- | A 'Transformer' that paints a 'Picture' in the color determined
-- by the given name.
paintIn :: Text -> Transformer Picture
paintIn c p = painted(p,c)

-- | A 'Transformer' that moves a 'Picture' to the right by the given
-- units.
rightBy :: Number -> Transformer Picture
rightBy v p = translated(p,(v,0))

-- | A 'Transformer' that rotates a 'Picture' counterclockwise by the
-- given degrees.
rotate :: Number -> Transformer Picture
rotate v p = rotated(p,v)

-- | A 'Transformer' that scales a 'Picture' by the given horizontal
-- and vertical scaling factors.
scale :: (Number,Number) -> Transformer Picture
scale(sx,sy) p = scaled(p,sx,sy)

-- | A 'Transformer' that moves a 'Picture' by the given displacement.
translate :: Point -> Transformer Picture
translate disp pic = translated(pic,disp)

-- | A 'Transformer' that moves a 'Picture' up by the given units.
upBy :: Number -> Transformer Picture
upBy v p = translated(p,(0,v))


