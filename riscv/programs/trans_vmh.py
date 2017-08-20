#!/usr/bin/env python

import sys

if len(sys.argv) != 3:
	print 'Usage: ./trans_vmh [input vmh] [output vmh]'
	raise

in_file = sys.argv[1]
out_file = sys.argv[2]

with open(in_file, 'r') as fin:
	lines = fin.readlines();

with open(out_file, 'w') as fout:
	fout.write(lines[0]);
	for i in xrange(1, len(lines)):
		val = lines[i][:-1].rstrip('\n');
		fout.write(val[8:] + '\n');
		fout.write(val[:8] + '\n');

