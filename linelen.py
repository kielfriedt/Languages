#
 #	Copyright (C) 2011  Kiel Friedt
 #
 #    This program is free software: you can redistribute it and/or modify
 #    it under the terms of the GNU General Public License as published by
 #    the Free Software Foundation, either version 3 of the License, or
 #    (at your option) any later version.
 #
 #    This program is distributed in the hope that it will be useful,
 #    but WITHOUT ANY WARRANTY; without even the implied warranty of
 #    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 #    GNU General Public License for more details.
 #
 #    You should have received a copy of the GNU General Public License
 #    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
import sys
for arg in sys.argv:
	txtfile = open(arg,"r")
	n = 1
	while 1:
		line=txtfile.readline()
		if not line: 
			break
		if (len(line) >= 80):
			# print("%s" % (arg)) 
			print("%s:%d\n>>%s" % (arg,n,line)) 
		n = n + 1
	txtfile.close()
