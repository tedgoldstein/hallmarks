#!/bin/bash
s='Models'
t='tmp'

i='../genelists/GO/data/hallmarks.genes'

mkdir -p $s $t

while read p q; do

  for h in `ls $i/`; do
    echo Hallmark $h
    echo p $p
    echo q $q

    if [ -f "$s/$p.to.$q.$h.signature" ]; 
    then
        echo "$s/$p.to.$q.$h.signature" exists 
    else
        echo making "$s/$p.to.$q.$h.signature" 

        # echo python2 bin/matrix_slice.py $i/$h "lists/TCGA.$q"  ReferenceData/TCGA.RSEM "$t/$q.$h.data"
        python2 bin/matrix_slice.py $i/$h "lists/TCGA.$q"  ReferenceData/TCGA.RSEM > "$t/$q.$h.data"

        # echo python2 bin/matrix_slice.py $i/$h "lists/GTEX.$p"  ReferenceData/GTEX.RSEM 
        python2 bin/matrix_slice.py $i/$h "lists/GTEX.$p"  ReferenceData/GTEX.RSEM > "$t/$p.$h.data"

        # echo python2 bin/matrix_join_common.py "$t/$p.$h.data" "$t/$q.$h.data" "$t/$p.to.$q.$h.data"
        python2 bin/matrix_join_common.py "$t/$p.$h.data" "$t/$q.$h.data" > "$t/$p.to.$q.$h.data"

        # echo Rscript bin/thresholdColumns.R "$t/$p.to.$q.$h.data"
        Rscript bin/thresholdColumns.R "$t/$p.to.$q.$h.data"

        # echo head -1 "$t/$p.to.$q.$h.data" 
        head -1 "$t/$p.to.$q.$h.data" | tr '\t' '\n' | sed -e '1d;s/GTEX.*/0/;s/TCGA.*/1/' > "$t/$p.to.$q.$h.phen"

        # echo cut -f1 "$t/$p.to.$q.$h.data"
        cut -f1 "$t/$p.to.$q.$h.data" | sed -e '1d' > "$t/$p.to.$q.$h.genes"

        # echo python2 bin/adjacency.py  ReferenceData/NETWORK "$t/$p.to.$q.$h.genes" 
        python2 bin/adjacency.py  ReferenceData/NETWORK "$t/$p.to.$q.$h.genes" | sed -e 's/ //g' > "$t/$p.to.$q.$h.network"

        # echo Rscript bin/gelNet.R "$t/$p.to.$q.$h.data" "$t/$p.to.$q.$h.phen" "$t/$p.to.$q.$h.network" "$s/$p.to.$q.$h_$h.signature" "$h" "$p" "$q"
        Rscript bin/gelNet.R "$t/$p.to.$q.$h.data" "$t/$p.to.$q.$h.phen" "$t/$p.to.$q.$h.network" "$s/$p.to.$q.$h.signature" "$h" "$p" "$q"
    fi

  done
done < lists/modellist
