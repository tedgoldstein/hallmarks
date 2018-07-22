
export R_LIBS=~/R/library

run: scores
	./runApp

scores: Signatures/signatures GeneLists/genes.all prepareR
	(cd Scores; make)

Signatures/signatures: GeneLists/genes.all prepareR
	(cd Signatures/ReferenceData; make)
	(cd Signatures; make)

GeneLists/genes.all: prepareR
	(cd GeneLists make)

prepareR : 
	Rscript install.R
	touch $@
