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
-- cs 381, Summer 2010, homework 2-1
-- Kiel Friedt
--
--Mini Logo is an extremely simplified version of the Logo language for programming 2D graphics. The idea
--behind Logo and Mini Logo is to describe simple line graphics through commands to move a pen from one
--position to another. The pen can either be “up” or “down”. Positions are given by pairs of integers. Macros
--can be defined to reuse groups of commands. The syntax of Mini Logo is as follows.
--
--prog ::= cmd ; prog |
--cmd ::= pen mode
--    | moveto (pos,pos)
--    | def name ( pars ) prog
--    | call name ( vals )
--mode ::= up | down
--pos ::= num | name
--pars ::= name, pars |
--vals ::= num, vals |

module hw1 where

-- abstract syntax for Mini Logo as a Haskell data type

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

--Mini Logo macro vector that draws a line from a given position (x1,y1) to a given position
--(x2,y2) and represent the macro in abstract syntax

-- 1.b)
vector = Def "vector" [x1, y1, x2, y2] [ pen down, moveto(x1,y1), moveto(x2,y2), pen up]

--Haskell function steps :: Int -> Prog that constructs a Mini Logo program which draws a
--stair of n steps.

--1.c)

steps :: Int -> Prog
steps 0 = []
steps n = [ pen up, moveto(I n, I n), pen down] ++ stairs(n)

stairs :: Int -> prog
stairs 0 = []
stairs n = [ moveto(I n-1, I n),moveto(I n-1, I n-1)] ++ stairs(n-1)

-- ______________________________________________________________________________________________________________________

--The “Digital Circuits Design Language” whose syntax is shown below can be used to describe circuits built
--from these gates.
--circuit ::= gates; links
--gates ::= num:gate ; gates | empty
--gate ::= and | or | xor | not
--links ::= from num.num to num.num; links | empty

--abstract syntax for the above language as a Haskell data type.

-- 2.a)

type Circuit = ( Gates, Links)
type Gates = [(Int,Gate)]
data Gate = And | Or |Xor | Not
type Links = [Link]
data Link  = L (Int, Int) (Int, Int)

--Represent a half adder circuit in abstract syntax

-- 2.b)
([[1, Xor], [2, And]], [ L (1,1) (2,1) , L ( 1,2) (2,2)])

--_____________________________________________________________________________________________________
--data Expr = N Int
--                | Plus Expr Expr
--                | Times Expr Expr
--                | Neg Expr
--

--Represent the expression -(3+4)*7 in the alternative abstract syntax

-- 3.a)

x =  Times(Neg((Plus (N 3) (N 4))) (N 7))

--advantages or disadvantages of either representation

-- 3.b) I found that the first abstract syntax was much easier to understand mostly
--    because thats what i have seen in class. But i think that once i understand
--    the second one it would be easier to write out.

--Define a function translate :: Expr -> Exp that translates expressions given in the first abstract syntax
--into equivalent expressions in the second abstract syntax

-- 3.c)

translate :: Expr -> Exp
translate ( N i)       = (N i)
translate (Plus l r)   = Apply Add [translate N l, translate N r]
translate (Times l r ) = Apply Multiply [translate N l, translate N r]
translate (Neg e)      = Apply Neg [translate e]
