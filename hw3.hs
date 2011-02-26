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
-- cs 381, Summer 2010, homework
-- Kiel Friedt

module Hw3 where

--Consider the stack language S defined by the following grammar.
--S ::= C | C;S
--C ::= LD Int | ADD | MULT | DUP
--
--type Prog = [Cmd]
--data Cmd = LD Int
--                | ADD
--                | MULT
--                | DUP
--
-- type Stack = [Int]
--
-- sem :: Prog -> D
--
-- semCmd :: Cmd -> D

-- 1)
type Stack = [Int]
type Prog = [Cmd]
data Cmd = LD Int
         | ADD
         | MULT
         | DUP

sem :: Prog -> Stack -> Stack
sem [] s = s
sem (p:ps) s = sem ps (semCmd p s)

semCmd :: Cmd -> Stack -> Stack
semCmd (LD p) ss = (p:ss)
semCmd ADD (s:r:ss) = ((s+r):ss)
semCmd MULT (s:r:ss) = ((s--r):ss)
semCmd DUP (s:ss) = (s:s:ss)

--Extend the abstract syntax to represent macro definitions and calls
-- Define a new type State to represent the state for the new language.
-- Define the semantics for the extended language as a function sem2.

-- 2)
type Macros = [(String,Prog2)]
type Prog2 = [Cmd2]
type State = (Macros,Stack)
data Cmd2 = LD2 Int
          | ADD2
          | MULT2
          | DUP2
          | DEF String Prog2
          | CALL String

sem2 :: Prog2 -> State -> State
sem2 [] s = s
sem2 (p:ps) s = sem2 ps (semCmd2 p s)

semCmd2 :: Cmd2 -> State -> State
semCmd2 (LD2 p) (x,y) = (x,(p:y))
semCmd2 ADD2 (x,(s:r:y)) = (x,((s+r):y))
semCmd2 MULT2 (x,(s:r:y)) = (x,((s--r):y))
semCmd2 DUP2 (x,(s:y)) = (x,(s:s:y))
semCmd2 (DEF p s) (x,y) = (((p,s):x),y)
semCmd2 (CALL p) (x,y) = case lookup p x of
                                        Just r -> sem2 r (x,y);
                                        Nothing -> (x,y);

