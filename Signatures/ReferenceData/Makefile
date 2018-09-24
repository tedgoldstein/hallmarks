
got.data: TCGA.RSEM GTEX.RSEM NETWORK HUGO NormalizedReferenceData.rda 

NormalizedReferenceData.rda : NormalizeReferenceData.R TCGA.RSEM GTEX.RSEM ../../GeneLists/genes.gmt 
	Rscript NormalizeReferenceData.R

TCGA.RSEM: get
	./get "https://toil.xenahubs.net/download/tcga_RSEM_Hugo_norm_count.gz" /data/OMF/RSEM/latest/TCGA.RSEM

GTEX.RSEM: get
	./get "https://toil.xenahubs.net/download/gtex_RSEM_Hugo_norm_count.gz" /data/OMF/RSEM/latest/GTEX.RSEM

# These are now checked into the repo.
#
# NETWORK: get
# 	./get "http://www.pathwaycommons.org/archives/PC2/v8/PathwayCommons.8.All.BINARY.SIF.hgnc.txt.sif.gz" NETWORK
# 
# HUGO: 
# 	wget "ftp://ftp.ebi.ac.uk/pub/databases/genenames/new/tsv/hgnc_complete_set.txt" -O HUGO
