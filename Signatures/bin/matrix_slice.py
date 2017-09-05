#!/usr/bin/python

import re
import sys

# Slice data from a tsv

if len(sys.argv) < 4:
    sys.stderr.write("Usage: matrix_slice.py rowlistfile collistfile filename\n")
    sys.exit(1)

rows          = set(open(sys.argv[1]).read().upper().split("\n"))
cols          = set(open(sys.argv[2]).read().upper().split("\n"))


def out(line):
    sys.stdout.write(line[0])
    for c in columns:
        sys.stdout.write("\t"+line[c])
    sys.stdout.write("\n")

columns = []
for line in open(sys.argv[3]):
    line = line[:-1].split("\t")
    if len(columns) == 0:
        for i in xrange(1, len(line)):
            if line[i] in cols:
               columns.append(i)
        out(line)
    else:
        if line[0].upper() in rows:
            out(line)
