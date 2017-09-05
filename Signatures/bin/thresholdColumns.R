args <- commandArgs(trailingOnly = TRUE)
X <- read.table(args[1], header=TRUE, row.names=1);
Y = X[,colSums(X^2) > 1000]

datafile <- file(args[1], open = 'wt')
cat("\t", file=datafile, fill=FALSE)
write.table(Y, file=datafile, sep="\t",  quote=F, row.names = T)
