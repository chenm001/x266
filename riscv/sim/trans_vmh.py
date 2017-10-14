#! /usr/bin/env python

import sys;

if len(sys.argv) != 5:
    print 'Usage: ./trans_vmh <input data vmh> <words> <banks> <prefix>'
    raise

in_file = sys.argv[1]
words = int(sys.argv[2])
banks = int(sys.argv[3])
prefix = sys.argv[4]

with open(in_file, 'r') as fin:
    lines = fin.readlines();

for i in xrange(banks):
    out_file = prefix + str(i);
    with open(out_file, 'w') as fout:
        fout.write('@0\n');
        for x in xrange(i, words, banks):
            fout.write(lines[x]);
