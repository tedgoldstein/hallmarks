library(data.table)
setwd("/Users/tgoldstein/GitHub/oct31/hallmarks/Signatures/ReferenceData")

need = readLines("../../GeneLists/genes.all")

rescale = function(b) {
  maxb = max(b)
  minb = min(b)
  cat("maxb=")
  cat(maxb);
  cat("\n")
  # if (!is.numeric(maxb)) browser()
  # if (!is.numeric(minb)) browser()
  
  c = 6/(maxb-minb)*(b-minb)-3
  d = scale(c, center = T, scale = T);
  return(d);
}

rank.normalize <- function(x, FUN=qnorm, ties.method = "average", na.action) {
    if (missing(na.action)) {
        na.action <- get(getOption("na.action"))
    }
    if(! is.function(na.action)) {
        stop("'na.action' must be a function")
    }
    x <- na.action(x)
    ret = FUN(rank(x, ties.method = ties.method)/(length(x)+1))
    ret
}

Genes <- NULL

read = function(nm) {
  dt = fread(nm)
  rn = dt[,1]
  dt = dt[,-1]

  normed = apply(dt, 2, function(x) scale(rank.normalize(x), scale=TRUE, center=TRUE))
  normed = as.data.frame(normed)
  colnames(normed) = colnames(dt)
  rownames(normed) = unlist(rn)
  normed
}


  
if (!exists("TCGA.RSEM"))
  print(system.time(TCGA.RSEM <-- read("TCGA.RSEM")))
if (!exists("GTEX.RSEM"))
  print(system.time(GTEX.RSEM <-- read("GTEX.RSEM")))


# genes = intersect( rownames(GTEX.RSEM), rownames(TCGA.RSEM))
# NormalizedReferenceData =  cbind(GTEX.RSEM[genes,], TCGA.RSEM[genes,])
NormalizedReferenceData =  cbind(GTEX.RSEM, TCGA.RSEM)

datafile <- file("NormalizedReferenceData.rda", open = 'wb')
save(NormalizedReferenceData, file=datafile)
close(datafile)

#datafile <- file("NormalizedReferenceData.txt", open = 'wt')
# X =  cbind(GTEX.RSEM[genes,], TCGA.RSEM[genes,])
# cat("\t", file=datafile, fill=FALSE)
# write.table(X, file=datafile, sep="\t",  quote=F, row.names = T)


