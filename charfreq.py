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
for arg in sys.argv[1:]
	txtfile = open(arg,"r")
	whole = txtfile.read()
	num = len(whole)
	cf_dic = {}
	for char in whole.lower():
    		cf_dic[char] = cf_dic.get(char, 0) + 1
 
	print "Characters sorted by ASCII number:"
	# create a sorted list of keys
	key_list = list(cf_dic.keys())
	key_list = sorted(key_list, key = lambda w1:cf_dic[w1], reverse = True)
	n = 1
	for key in key_list:
	    # don't show space and newline
    		if key not in "\,\.\;\:\'\" \n":
        	# associate the value with the key
			freq = float(float(cf_dic[key]) / float(num))*100
			print("%s %10d (%5.2f %%)" % (key, cf_dic[key],freq))
			n = n + 1
			if n > 20: break
