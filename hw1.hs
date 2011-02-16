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
-- cs 381, Summer 2010, homework 2
-- Kiel Friedt

module hw1 where

-- 1.a)
prog = [Cmd]
data Cmd = pen mode
	 | moveto = (pos,pos)
         | def string Pars prog
         | call string Vals

data Mode = Up | Down
data Pos = I Int | S String
type Pars = [String]
type Vals = [Int]

-- 1.b)
vector = Def "vector" [x1, y1, x2, y2] [ pen down, moveto(x1,y1), moveto(x2,y2), pen up]

--1.c)

steps :: Int -> Prog
steps 0 = []
steps n = [ pen up, moveto(I n, I n), pen down] ++ stairs(n)

stairs :: Int -> prog
stairs 0 = []
stairs n = [ moveto(I n-1, I n),moveto(I n-1, I n-1)] ++ stairs(n-1)

-- 2.a)

type Circuit = ( Gates, Links)
type Gates = [(Int,Gate)]
data Gate = And | Or |Xor | Not
type Links = [Link]
data Link  = L (Int, Int) (Int, Int)

-- 2.b)
([[1, Xor], [2, And]], [ L (1,1) (2,1) , L ( 1,2) (2,2)])

-- 3.a)

x =  Times(Neg((Plus (N 3) (N 4))) (N 7))

-- 3.b) I found that the first abstract syntax was much easier to understand mostly
--    because thats what i have seen in class. But i think that once i understand
--    the second one it would be easier to write out.

-- 3.c)

translate :: Expr -> Exp
translate ( N i)       = (N i)
translate (Plus l r)   = Apply Add [translate N l, translate N r]
translate (Times l r ) = Apply Multiply [translate N l, translate N r]
translate (Neg e)      = Apply Neg [translate e]
