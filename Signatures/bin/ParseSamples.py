#
# Parses TCGA and GTEX clinical sample .phenotype files and creates the temporary data in the lists directory
#

from __future__ import print_function

import re, sys


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

for c in open("MapControlToDiseaseTissue"):
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
    for phe in open("ReferenceData/" + general + ".phenotype"):
        processLine(phe, general)

with open("lists/modellist", "w") as f:
    for pat  in pats:
        f.write(mangle(pat["base"]) + "\t" + mangle(pat["cancer"]) + "\n")


for specific, samplelist in samplelists.iteritems():
    with open("lists/"+specific, "w") as f:
        print("Writing sample list:",  specific);
        for l in sorted(list(samplelist)):
            f.write(l)
            f.write("\n");
sys.exit(0);
