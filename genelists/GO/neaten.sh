awk '{print ($4 == "-" ? $2 : $4)}'< "data/hallmarks/$1"  |  tr '[a-z]' '[A-Z]'  | sort | uniq | sort -n  > "data/hallmarks.genes/$1"
