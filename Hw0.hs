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
-- cs 381, Summer 2010, homework 0
-- kiel friedt
module Hw0 where


hello :: String
hello = "Hello World!"

-- betweenOneAndTen that returns True if its argument is between 1 and 10 inclusive.

betweenOneAndTen :: Int -> Bool
betweenOneAndTen x = x <= 10 && x >= 1

--Type signature for a function inInterval, which returns True
if the third argument is in between the first two (inclusively).

inInterval :: Int->Int->Int->Bool
inInterval lo hi x = (x >= lo && x <= hi)

-- betweenOneAndTwenty using inInterval function

betweenOneAndTwenty :: Int->Bool
betweenOneAndTwenty x = inInterval 1 20 x

--takes in a int value and returns value in polynomial 

poly :: Int->Int
poly x = 4*x*x + 2*x +17

--Factorial recurrence relation.

fact :: Integer->Integer
fact 0 = 1 
fact n = n * fact (n-1)

-- two functions: pick and choose using fact 

pick :: Integer->Integer->Integer
pick n k = fact n `div` fact (n - k)

choose :: Integer->Integer->Integer
choose n k = fact n `div` (fact (n-k) * fact k)

--Guard Expressions

rec :: Int->Int 
rec 0 = 11 
rec 1 = 22
rec n | even n	  = n * rec(n-1) + rec(n-1) 
        | otherwise  = rec(n-1) * rec(n-2)

mod1 :: Int->Int->Int
mod1 n x = mod n x

-- type signature and function rec2 for the recurrence

rec2 :: Int->Int
rec2 n | n == 0 = 44
       | ((mod n 3) == 0 && (mod n 2) /= 0) = n*n*rec2(n-1)
       | otherwise = n*rec2(n-1)

--function rec3 for the same recurrence as rec2 except use a guard expressions and no if-then-else
expressions.

rec3 :: Int->Int
rec3 0 = 44
rec3 n | ((mod n 3) == 0 && (mod n 2) /= 0) = n*n*rec2(n-1)
rec3 n = n*rec2(n-1)

-- squares 15

fifteenSquared :: Int
fifteenSquared = 15 * 15