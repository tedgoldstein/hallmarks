


signatures : lists/modellist  ../GeneLists/data/hallmarks.genes
	bash  ./bin/buildSignatures.sh  ./GeneLists/data/hallmarks.genes
	Rscript ./bin/processSigs.R Models/*.signature

lists/modellist : step1 MapControlToDiseaseTissue
	mkdir -p lists
	python ./bin/ParseSamples.py

step1 ./ReferenceData/NETWORK ./ReferenceData/GTEX.RSEM ./ReferenceData/GTEX.phenotype ./ReferenceData/TCGA.RSEM ./ReferenceData/TCGA.phenotype :
	(cd ReferenceData; make)
	touch step1
