# log2(x+1) normalize the data table found in args[1]

library(RJSONIO)


args <- commandArgs(trailingOnly = TRUE)

printf <- function(...) cat(sprintf(...))


# load the data
X <- read.table(args[[1]], as.is=TRUE, header=FALSE);

X.length = dim(X)[1]
X.width = dim(X)[2]

X.data = X[,2:(X.width-1)]
X.rownames = X[2:X.length,X.width]

X.colnames = X.data[1,]
X.data = X.data[2:X.length,]

X.matrix = data.matrix(X.data)
X.log2plus1 = log2(X.matrix+1)
rownames(X.log2plus1) = X.rownames
rownames(X.colnames) = c("gene");


out = file(paste0(args[[1]],".log_norm"), open="w");
write.table(X.colnames, col.names=F, file=out, sep="\t", quote=FALSE);
write.table(X.log2plus1, col.names=F, file=out, sep="\t", quote=FALSE);
close(out)
