#!/usr/bin/python

import sys
import csv
import pdb

def read_input(name):
    reader = csv.reader(open(name, "rb"), delimiter='\t')
    first = reader.next()[1:];
    rest = {row[0]: row[1:] for row in reader}
    return first, rest

a = read_input(sys.argv[1])
b = read_input(sys.argv[2])
a_dict = a[1];
b_dict = b[1];
a_genes = set(a_dict.keys())
b_genes = set(b_dict.keys())
common_genes = sorted(list(a_genes.intersection(b_genes)))


writer = csv.writer(sys.stdout, delimiter='\t', lineterminator='\n');
line = ["Gene"]+ a[0] + b[0];
writer.writerow(line)

for gene in common_genes:
    line = [gene]+ a_dict[gene] + b_dict[gene];
    writer.writerow(line)
