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

betweenOneAndTen :: Int -> Bool
betweenOneAndTen x = x <= 10 && x >= 1

inInterval :: Int->Int->Int->Bool
inInterval lo hi x = (x >= lo && x <= hi)

betweenOneAndTwenty :: Int->Bool
betweenOneAndTwenty x = inInterval 1 20 x

poly :: Int->Int
poly x = 4*x*x + 2*x +17

fact :: Integer->Integer
fact 0 = 1 
fact n = n * fact (n-1)

pick :: Integer->Integer->Integer
pick n k = fact n `div` fact (n - k)

choose :: Integer->Integer->Integer
choose n k = fact n `div` (fact (n-k) * fact k)

rec :: Int->Int 
rec 0 = 11 
rec 1 = 22
rec n | even n	  = n * rec(n-1) + rec(n-1) 
      | otherwise = rec(n-1) * rec(n-2)

mod1 :: Int->Int->Int
mod1 n x = mod n x

rec2 :: Int->Int
rec2 n | n == 0 = 44
       | ((mod n 3) == 0 && (mod n 2) /= 0) = n*n*rec2(n-1)
       | otherwise = n*rec2(n-1)

rec3 :: Int->Int
rec3 0 = 44
rec3 n | ((mod n 3) == 0 && (mod n 2) /= 0) = n*n*rec2(n-1)
rec3 n = n*rec2(n-1)


fifteenSquared :: Int
fifteenSquared = 15 * 15