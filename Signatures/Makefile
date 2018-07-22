


signatures : lists/modellist  ../GeneLists/data/hallmarks.genes # step1
	mkdir -p Models
	Rscript ./bin/gelNet.R 
	Rscript ./bin/processSigs.R Models/*.signature

lists/modellist : step1 MapControlToDiseaseTissue
	mkdir -p lists
	python2.7 bin/ParseSamples.py

#step1 ./ReferenceData/NETWORK ./ReferenceData/GTEX.RSEM ./ReferenceData/GTEX.phenotype ./ReferenceData/TCGA.RSEM ./ReferenceData/TCGA.phenotype :
#	(cd ReferenceData; make)
#	touch step1
