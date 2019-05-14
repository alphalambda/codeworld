{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

module Extras.Game.Hanoi (solve,move,solveWith,Move)
where

import Prelude
import Extras((<?>),(<$>),(<|),messages,formatting,iterated,cumsum,printedNumbers)


-- traced(x,_) = x

-- Towers of Hanoi

countIllegalMoves = True

program = solve(10,100)

-------------------------------------------------------------------------------
-- Solvers
-------------------------------------------------------------------------------

solver(1,src,dst,_)    = [move(src,dst)]

solver(n,src,dst,[o1]) = solver(n-1,src,o1,[dst])
                      ++ solver(1,src,dst,[])
                      ++ solver(n-1,o1,dst,[src])

solver(n,src,dst,o1:other)
  | isLevel   = combo(k-1,src,dst,o1:other)
  | otherwise =  solver(n-k,src,o1,dst:other)
              ++ solver(k,src,dst,other)
              ++ solver(n-k,o1,dst,src:other)
    where
    order = 1+length(other)
    discriminant = csum#order
    r = iroot(discriminant,n)
    k0 = csum#(order-1)#r
    k1 = csum#(order-1)#(r+1)
    n0 = csum#order#r
    n1 = csum#order#(r+1)
    k = truncation(k0 + (n-n0)/(n1-n0) * (k1-k0))
    isLevel = discriminant#k == n
    l = level(n,1+length(other))
    kl = truncation(l)

    level(num,2) = (sqrt(1+8*num)-1)/2
    level(num,len) = num

-- r = iroot(vec,n) means vec#r <= n < vec#(r+1)
iroot(vec,n) = go(0,vec)
  where
  go(index,x:xs) | n < x = index
                 | otherwise = go(index+1,xs)
                 
-- r = iroot'(vec,n) means vec#(r-1) < n <= vec#r
iroot'(vec,n) = go(1,vec)
  where
  go(index,x:xs) | n <= x = index
                 | otherwise = go(index+1,xs)
                 
csum = iterated(cumsum,[1..])
    
combo(0,src,dst,_) = [move(src,dst)]
combo(1,src,dst,other) = moveSub(src,dst,other)

combo(level,src,dst,other) =
    spread(src,other ++ [dst])
    ++
    regroup(dst,src,other)
    where
    spread(_,[]) = []
    spread(src,dst:other) = combo(level-1,src,dst,other) ++ spread(src,other)
    regroup(_,_,[]) = []
    regroup(dst,xtra,src:other) = regroup(dst,xtra,other) ++ combo(level-1,src,dst,xtra:other)

moveSub(src,dst,other) =
  (spread <$> range)
  ++
  [ move(src,dst) ]
  ++
  (regroup <$> reversed(range))
  where
  range = [1..length(other)]
  spread(i) = move(src,other#i)
  regroup(i) = move(other#i,dst)

-------------------------------------------------------------------------------
-- Configuration: Number of discs and moves
-------------------------------------------------------------------------------

-- | @solveWith(pegs,discs,moves)@ attempts to solve the Tower of Hanoi puzzle
-- for the given numbers of @pegs@ and @discs@ using the given @moves@
solveWith :: (Number,Number,[Move]) -> Program
solveWith(npegs,ndiscs,givenMoves) = interactionOf(start,step,input,draw)
  where
  start _ = Model
    { total = 0
    , timer = 0
    , frameDelay = 0
    , speedUp = 1+quotient(ndiscs,10)
    , numDiscs = ndiscs
    , numPegs = npegs
    , moves = givenMoves
    , pegs = (ndiscs,[1..ndiscs]) : first(repeating([(0,[])]),npegs-1)
    }


-- | @solve(pegs,discs)@ solves the Tower of Hanoi puzzle
-- for the given numbers of @pegs@ and @discs@
solve :: (Number,Number) -> Program
solve(npegs,ndiscs) = solveWith(npegs,ndiscs,solver(ndiscs,1,npegs,[2..npegs-1]))

-------------------------------------------------------------------------------
-- DO NOT MODIFY THE CODE BELOW THIS LINE
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Game logic
-------------------------------------------------------------------------------

type Move = [Peg] -> [Peg]
type Peg = (Number,[Number])

data Model = Model
  { total :: Number
  , timer :: Number
  , frameDelay :: Number
  , speedUp :: Number
  , numDiscs :: Number
  , numPegs :: Number
  , moves :: [Move]
  , pegs :: [Peg]
  }


count = firstOfPair
peg = secondOfPair

step (model@Model{..},dt) =
  if timer >= frameDelay
  then multi(speedUp,model)
  else model { timer = timer + dt }

multi(0,model) = model
multi(n,model) = multi(n-1,single(model))

single(model@Model{..}) =
    if empty(moves) then model
    else model { total = newTotal
               , timer = 0
               , moves = rest(moves,1)
               , pegs = newPegs
               }
    where
    newTotal
      | countIllegalMoves = total + 1
      | otherwise = if oldCount == newCount then total else total + 1
    newPegs = (moves#1)(pegs)
    oldCount = count <$> pegs
    newCount = count <$> newPegs
    
input (model,_) = model

-------------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------------

draw(Model{..})
  | numPegs > 18 = drawMany(Model{..})
  | numPegs > 6 = drawInfos(Model{..})
  | otherwise = pegPics & translated(lettering(printed(total)),-8,9)
  where
  pegPics = pictures[translated(drawPeg i s numDiscs,x,y)
                      | i <- [1..]
                      | s <- pegs
                      | y <- [2.5,-7.5]
                      , x <- [-5,0,5]
                      ]

drawMany(Model{..}) = translated(lettering(printed(total)<>" moves"),0,9.5)
  & pictures([bar(i,count(p)) | i <- [1..] | p <- pegs])
    where
    bar(i,n) = translated(solidRectangle(w,rh),(i-0.5)*w-10,rh/2-10)
        where
        rh = n * h
    w = 20/numPegs
    h = 18/numDiscs


drawInfos(model) = resized(messages(movesInfo : "" : numInfos),2)
  & pictures[translated(solidRectangle(w,1),w/2,8.5-i)
            | i <- [1..]
            | s <- pegs(model), let w = 9*count(s)/numDiscs(model)
            ]
  where
  cons(f,g)(x) = f(x) : g(x)
  movesInfo = format[ lit "Num Moves: "
                    , dec(total,8,0)
                    ] model
  numInfos = print <$> [(i,count(s)) | i <- [1..] | s <- pegs(model)]
  print = format[lit "Peg",dec(snum,3,0),lit ": ",dec(slen,5,0)]
    where snum = firstOfPair
          slen = secondOfPair
  (format,spc,lit,txt,dec) = formatting


drawPeg n (num,discs) numDiscs =
  pictures[translated(disc i,0,(0.5+num-pos-1)*height)
          | i <- discs
          | pos <- [0..]
          ]
  & thickPolyline([(-2,-0.2),(2,-0.2)],0.2)
  & thickPolyline([(0,-0.2),(0,totalheight)],0.2)
  & translated(lettering("Peg " <> (printed n)),0,-1)
  & translated(dilated(lettering(printed(num)),0.75),0,-1.9)
  where
  width = 3.5 / numDiscs
  height = totalheight/(numDiscs+1)
  totalheight = 7
  disc i =
    colored(solidRectangle(width*i,height),assortedColors#i)
    & border
    where
    border | numDiscs < 20 = thickRectangle(width*i,height,0.2)
           | otherwise = rectangle(width*i,height)

-------------------------------------------------------------------------------
-- Manipulation of the pegs
-------------------------------------------------------------------------------

top n pegs = peg(pegs#n)#1

pop n pegs = [if i == n && c > 0 then (c-1,rest(s,1)) else cs
             | i <- [1..]
             | cs@(c,s) <- pegs
             ]

push(n,x) pegs = [if i == n then (c+1,x:s) else cs
                 | i <- [1..]
                 | cs@(c,s) <- pegs
                 ]

emptyPeg n pegs = count(pegs#n) == 0

-- | A move represents the action of moving a disc from the given source peg
-- to the given destination peg
move :: (Number,Number) -> Move
move(i,j)(pegs) =
  if emptyPeg i pegs then pegs
  else if emptyPeg j pegs then moveit
  else if x < top j pegs then moveit
  else pegs
  where
  x = top i pegs
  moveit = pop(i) <| push(j,x) <| pegs

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

resized(pic,fac) = translated(dilated(translated(pic,10,-10),fac),-10,10)