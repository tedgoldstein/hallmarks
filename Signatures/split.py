#
# Parses TCGA and GTEX clinical sample .phenotype files and creates the temporary data in the lists directory
#

from __future__ import print_function

import re, sys, pdb


def mangle(specific):
    specific = specific.replace(" ", "_")
    specific = specific.replace("|", "_or_")
    specific = specific.replace("__", "_")
    return specific

    

def parseExpr(e):
    e = e.replace("_", " ");
    orExp = ["\\b" + ("\\b.*\\b".join(a.split())) + "\\b"  for a in e.split("|")]
    return [e for e in orExp];


pats = []

for c in open("../Signatures/MapControlToDiseaseTissue"):
    c = c[:-1].split("\t");
    d = {
        "base": c[0],
        "cancer": c[1],
        "GTEX": parseExpr(c[0]),
        "TCGA": parseExpr(c[1])
    }
    pats.append(d)


samplelists = {}

def processLine(phe, general):
    for pat in pats: 
        for e in pat[general]:
            if re.search(e, phe, re.IGNORECASE):
                sample = phe.split("\t")[0];
                if general == "TCGA":
                    specific = general + "." + mangle(pat["cancer"])
                else:
                    specific = general + "." + mangle(pat["base"])
                if not specific in samplelists:
                    samplelists[specific] = set();
                samplelists[specific].add(sample)

for general  in ["GTEX", "TCGA"]:
    for phe in open("../ReferenceData/" + general + ".phenotype"):
        processLine(phe, general)


sampleToFileMap = {}
generalToFilesMap = {"TCGA":[], "GTEX":[]}

for specific, samplelist in samplelists.iteritems():
    f = open("Split/" + specific + ".txt", "w")
    general = specific[0:4]
    generalToFilesMap[general].append(f)
    for sample in samplelist:
        sampleToFileMap[sample] = f

for general in ["GTEX", "TCGA"]:
    columnToFileMap = None
    files = generalToFilesMap[general]
    for l in open(general + ".Z"):
        l = l[0:-1].split("\t")
        if columnToFileMap == None:
            columnToFileMap = [None] * len(l)
            for i in xrange(1,len(l)):
                sample = l[i]
                if  sample in sampleToFileMap:
                    columnToFileMap[i] = sampleToFileMap[sample]

        for f in files:
            try:
                f.write(l[0])
            except:
                pdb.set_trace()

        for i in xrange(1,len(l)):
            f = columnToFileMap[i]

            if f:
                f.write("\t")
                f.write(l[i])

        for f in files:
            f.write("\n")

    for f in files:
        f.close()
