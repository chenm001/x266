#! /usr/bin/env python

import sys;

if sys.argv[1].endswith('K'):
   bytestoread = int(sys.argv[1].translate(None, 'K')) * 1024;
elif sys.argv[1].endswith('M'):
   bytestoread = int(sys.argv[1].translate(None, 'M')) * 1024 * 1024;
elif sys.argv[1].endswith('G'):
   bytestoread = int(sys.argv[1].translate(None, 'G')) * 1024 * 1024 * 1024;

while True:
   buffer = sys.stdin.read(bytestoread);
   if buffer == '':
      exit();
   sys.stdout.write(buffer);
   buffer = None;   # This one is for making sure the read buffer will get destroyed.
