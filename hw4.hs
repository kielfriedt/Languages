--
 --	Copyright (C) 2011  Kiel Friedt
 --
 --    This program is free software: you can redistribute it and/or modify
 --    it under the terms of the GNU General Public License as published by
 --    the Free Software Foundation, either version 3 of the License, or
 --    (at your option) any later version.
 --
 --    This program is distributed in the hope that it will be useful,
 --    but WITHOUT ANY WARRANTY; without even the implied warranty of
 --    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 --    GNU General Public License for more details.
 --
 --    You should have received a copy of the GNU General Public License
 --    along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
-- cs 381, Summer 2010, homework 4
-- Kiel Friedt

module Hw4 where

-- 1a)
type Stack = [Int]
type Prog = [Cmd]
data Cmd = LD Int 
         | ADD 
         | MULT 
         | DUP
         | INC 
         | SWAP 
         | POP Int

type Rank = Int 
type CmdRank = (Int,Int)

rankC :: Cmd -> CmdRank
rankC (LD i) = (0,1)
rankC ADD = (2,1)
rankC MULT = (2,1)
rankC DUP = (1,2)
rankC INC = (1,1)
rankC SWAP = (2,2)
rankC (POP i) = (i,0)

-- rankP uses rank
rankP :: Prog -> Maybe Rank
rankP [] = Just 0
rankP  p = rank p 0 

rank :: Prog -> Rank -> Maybe Rank
rank [] r = Just r
rank (p:ps) r =  let (a,b) = rankC p in
		 if a > r then Nothing else rank ps ((r-a)+b)

-- 1b)
tc :: Prog -> Maybe Stack
tc p = let d = rankP p in
	if d == Nothing then Nothing else Just (sem p)

sem :: Prog -> Stack
sem = foldl (flip semCmd) []

semCmd :: Cmd -> Stack -> Stack
semCmd (LD i) s = (i:s)
semCmd ADD (x:y:s) = ((x+y):s)
semCmd MULT (x:y:s) = ((x--y):s)
semCmd DUP (x:s) = (x:x:s)
semCmd INC (x:s) = ((x+1):s)
semCmd SWAP (x:y:s) = (y:x:s)
semCmd (POP i) s = (drop i) s

-- 2a)
-- 1) f is a list either y is a single element list or x is a list
--    g is a list either a empty list or y is a single element list

-- 2) well if f the list x is null or empty then it will return a sinle 
--    element list or else it will return the list x. where as with g
--    if the list x is null or empty then it will return a list of one 
--    element y or if the list is  not null then it will return a empty list 

-- 3) the empty list is more general since a empty list can be of any type

-- 4) well f has to be a list of a specifc type, where as g can be a list 
--    of some a specific type or of any type, doesnt matter.

-- 2b) function h's parameters are a list of b and a list of pairs a and b which
--     returns a list of b.

-- 2c) function k's parameters are a function call that receives a and returns
--     a, b and another function call that calls the first function call and
--     returns a, b and then call it again and returns a, a and then k returns
--     a, b. 

-- 2d) g :: a -> b or a function g with parameters that it receives a data 
--     type and returns another data type.

