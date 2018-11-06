


signatures : lists_geneID/modellist  ../GeneLists/data/hallmarks.geneID # step1
#	mkdir -p Models
	mkdir -p Models_geneID
	Rscript ./bin/gelNet.R 
	Rscript ./bin/processSigs.R Models_geneID/*.signature

lists_geneID/modellist : step1 MapControlToDiseaseTissue
#	mkdir -p lists
	mkdir -p lists_geneID
	python2.7 ./bin/ParseSamples.py

#step1 ./ReferenceData/NETWORK ./ReferenceData/GTEX.RSEM ./ReferenceData/GTEX.phenotype ./ReferenceData/TCGA.RSEM ./ReferenceData/TCGA.phenotype :
#	(cd ReferenceData; make)
#	touch step1
