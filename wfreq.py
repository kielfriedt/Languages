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
import sys, re
for arg in sys.argv[1:]:
	txtfile = open(arg,"r")
	whole = txtfile.read()
print whole
result = re.sub(r'[a-zA-Z]#\'\\1?[a-zA-Z]?', 'A', whole)
print result
