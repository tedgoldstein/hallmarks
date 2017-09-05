# hallmarks
Oncology Model Fidelity Score based on the Hallmarks of Cancer

See http://oncologymodels.org/AACR-2017-poster.pdf for full text

BACKGROUND: How can we compare animal models (GEMM, PDX) to human cancer?
Animal models remain a cornerstone of research efforts in Oncology to model the complexity of cancer progression and to discover new therapeutic approaches to disease management. With the advent of new genomic manipulation techniques such as CRISPR, and advances in mouse modeling with genetically engineered mouse models (GEMM) and patient-derived xenografts (PDX), we can expect the development of novel and powerful animal models in the near future. As part of the NCI Oncology Model Forum Project, we are developing a new scoring system called the Oncology Model Fidelity Scores (OMF score), which allow researchers and clinicians to compare animal models and select those most suited for the question at hand, whether in basic science, for translational research, or within clinical applications.
METHODS:
The formalism and tools we are developing for the analysis of mouse models and other model organisms are based on the Hallmarks of Cancer6,7 and therapeutic pathways as well as The Cancer Genome Atlas5 (TCGA), GTEX9 and other comprehensive systems. The scoring system begins with RNASEQ expression data analysis, and is designed for use in mapping animal models both to individual patients and to a TCGA cohort. We are applying this scoring system to mouse models1,3,4,10 available through NCI’s Oncology Model Forum. Our goal is to understand how these animal models compare to human cancers represented by TCGA data. We have normalized the scores to the 0-1000 range, where scores less than 500 indicate gene signatures that correlate with tissue-specific normal tissue, and scores greater than 500 are indicative of gene signatures that correlate with tissue-specific cancer tissue. The Hallmark-based technique conveys an overall understanding of the constitutive pathway-based gene sets that contribute to cancer.
Deriving Scores
Using Generalized Elastic Nets11, we learn the linear function: h(x) = wT φ(x) + b , where w is the set of weights, b is the bias, x is scaled normalized RNASEQ data. We learn the weights by comparing MSigDB8 Hallmark gene sets in GTEX3 tissue (yi = 0) vs TCGA4 tissue (yi = 1), and minimizing the likelihood function L:
where d and P are additional penalty weights for individual features and pairs of features, respectively. These provide domain knowledge, such as network adjacency. Setting dj = 1, P to the identity matrix produces the traditional elastic net. Scores are then mapped into 200-800 rangefor discussion purposes and Radar presentation, though it is expected that samples will exceed this range.
Processing Experimental Data from Mice
We extracted raw RNAseq data (SRA), mapped reads to the latest REFSEQ transcripts, and normalized abundance counts (TPM) by Kallisto2. Gene orthologs of human-mouse are based on the latest NCBI data. We took the log2(TPM+1) of summed counts, then normalized to a centered, fixed z score range to impose consistency.

RESULTS:
The OMF score allows us to determine which features of human cancer (as defined by the Hallmarks of Cancer) are recapitulated by the mouse model. To demonstrate the score, we reanalyzed data from a recent paper by Chuang and colleagues3 who used a GEMM of metastatic lung adenocarcinoma that has conditional alleles of KrasG12D and Trp53 (inactivating). Samples were also taken from mice at progressive
stages of malignancy (normal tissue, hyperplasia, primary tumors, metastases) and analyzed by RNASEQ.

CONCLUSIONS:
Animal models are central to our basic and translational research mission to treat and cure cancer, but we should not expect these models to recapitulate all aspects of human cancer. With the first comprehensive catalog of gene expression in healthy tissue and cancer tissues from GTEX and TCGA, we now have a foundation to create metrics such as the Oncology Model Fidelity Score that will enable us to better understand the benefits and limitations of individual preclinical models. Ultimately, the OMF Score will help to advance patient care through efficient identification and validation of animal models for a variety of applications, from pre-clinical testing of novel therapeutics to the use of patient-specific animal models.


Source Code:

genelists folder contains the various ways we are experimenting to generate gene list


