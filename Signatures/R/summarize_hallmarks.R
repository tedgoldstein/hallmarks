# Summarize hallmarks to min,mean,max

args <- commandArgs(trailingOnly = TRUE)
weights = read.table("hallmark.weights", row.names=1, header=T);

X = read.table(args[1], row.names=1, header=T);
row.names(X) <- gsub("HALLMARK_", "", row.names(X))
common = intersect(row.names(weights), row.names(X))
XX = X[common,]
w = weights[common,]

XX = data.matrix(XX)
w = data.matrix(w)
hallmarks = t(XX) %*% w

outf = paste0(gsub(".txt", "", args[1]),".hallmarks.txt")
write.table(t(hallmarks), file=outf, sep="\t")

summary = data.frame(
    mean=apply(hallmarks,2,mean),
    min=apply(hallmarks,2,min),
    max=apply(hallmarks,2,max))

outf = paste0(gsub(".txt", "", args[1]),".summary")
write.table(t(summary), file=outf, sep="\t")
