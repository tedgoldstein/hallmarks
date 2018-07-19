

foo:  prepareR
	(cd GeneLists make)
	(cd Signatures/ReferenceData; make)
	(cd Signatures; make)
	(cd Scores; make)


export R_LIBS=~/R/library

prepareR : 
	Rscript install.R
	touch $@
