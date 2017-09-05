#!/usr/bin/python

# Converts pathwaycommons triple tuples into a 0/1 adjency matrix 
# adjacency.py pathwayscommons.triples.network gene.subset.list > output

import sys
import csv
import pdb

reader = csv.reader(open(sys.argv[1], "rb"), delimiter='\t')
network = set()
genes = set()
for row in reader:
    genes.add(row[0])
    genes.add(row[1])
    network.add(row[0]+ " " +row[2])
    network.add(row[2]+ " " +row[0])

restrict = set(open(sys.argv[2]).read().split("\n"))

genes = genes.intersection(restrict)

genes = sorted(list(genes))

print "Gene",
for gene1 in genes:
    print "\t"+gene1,
print ""

for gene2 in genes:
    print gene2,
    for gene1 in genes:
        if gene1 + " " + gene2 in network:
            print "\t1",
        else:
            print "\t0",
    print ""
