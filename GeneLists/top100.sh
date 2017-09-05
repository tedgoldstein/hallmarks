awk '{print ($4 == "-" ? $2 : $4)}'< "data/hallmarks/$1"  |  tr '[a-z]' '[A-Z]'  | sort | uniq -c | sort -rn | head -100 | awk '{print $2}' > "data/hallmarks.genes/$1"
