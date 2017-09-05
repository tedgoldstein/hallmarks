
for i in $(ls data/hallmarks.genes/); do
  echo $i hallmark `awk '{print $1}' < data/hallmarks.genes/$i | sort | tr '\n' '\t'` | tr ' ' '\t'
done
                                      

