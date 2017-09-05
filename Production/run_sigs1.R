#!/usr/local/bin/Rscript

# Run linear equations with coeficients specified in 
# expects there to be a file in current directory named signtures,  json file producted by
# load and unpack the signatures for this specific cancer (specified in args[1]) 
# read expression data in file  in args[2]
# write output to  file=args[2].score

library(RJSONIO)

args <- commandArgs(trailingOnly = TRUE)
# args = c("lung_adenocarcinoma")
# args = c("colon_adenocarcinoma", "colon_adenocarcinoma.z")

printf <- function(...) cat(sprintf(...))

rank.normalize <- function(x, FUN=qnorm, ties.method = "average", na.action) {
    if (missing(na.action)) {
        na.action <- get(getOption("na.action"))
    }
    if(! is.function(na.action)) {
        stop("'na.action' must be a function")
    }
    x <- na.action(x)
    FUN(rank(x, ties.method = ties.method)/(length(x)+1))
}


# load and unpack the signatures for this specific tissue (specified in args[1])
signatures <- fromJSON("../Signatures/signatures");

index = list();
for (i in 1:length(signatures$signatures)) {
    cancer = signatures$signatures[[i]]$cancer
    if (!(cancer %in% names(index))) {
        index[[ cancer ]] = list();
    }
    index[[ cancer ]] = c(index[[ cancer ]], i);
}

cancer <- gsub(" ", ".", args[1]);

ix = index[cancer]
signatures <- signatures$signatures[unlist(ix)]



reference = list();

process = function() {

    # load the data
    X <- read.table(args[[2]], header=TRUE, row.names=1)
    possible = row.names(X)

    X = apply(X, 2, function(x) scale(rank.normalize(x), scale=TRUE, center=TRUE))
    row.names(X) <- possible
    X <- data.frame(X)
    scores = data.frame()
    
    reference = NULL;

    for (i in 1:length(signatures)) {
        signature    <- signatures[[i]];
        hallmark <- signature$hallmark;
        
        ref = data.frame(signature$reference$samples,signature$reference$score);
        colnames(ref) = c("samples", hallmark);
        if (is.null(reference)) {
          reference = ref
        } else {
          reference = merge(x = reference, y = ref, by="samples", all = TRUE)
        }
        
        should  <- names(signature$w)
        genes    <- intersect(should, possible)
        printf("%s %3.0f%%\n", hallmark, (length(genes)/length(should))* 100.0);
        if (TRUE  || length(genes) == length(should)) {
          score = data.frame();
          posScale <- signature$posScale;
          negScale <- signature$negScale;
          w = signature$w[genes]
      
          XX <- t(X[genes,])
          #cat(XX);
      
      
          raw = XX %*% w + signature$b;
          heat= XX * w + signature$b;
      
          for (j in 1:length(raw)) {
              value = raw[j];
              if (value < 0) {
                  score[1,j] = round(500  - (negScale * raw[j]));
              } else {
                  score[1,j] = round( (posScale * raw[j]) + 500);
              }
          }
          colnames(score) = colnames(X);
          row.names(score) = signature$hallmark;
          scores = rbind(scores, score);
          # return(scores);
       }
    }
    return (scores);
}

Hallmarks = unlist(lapply(signatures, 
    function(l) return(l[["hallmark"]])
))

# scores = cbind(Hallmarks=Hallmarks, scores)

scores = process()
scores.colnames = t(data.frame(colnames(scores)));


out = file(paste0(args[[2]],".score1"), open="w");
cat(paste0(args[[2]], "\t"), file=out)
write.table(scores.colnames, row.names=F, col.names=F, file=out, sep="\t", quote=FALSE);
write.table(scores, col.names=F, file=out, sep="\t", quote=FALSE);
close(out)



rank.normalize <- function(x, FUN=qnorm, ties.method = "average", na.action) {
    if (missing(na.action)) {
        na.action <- get(getOption("na.action"))
    }
    if(! is.function(na.action)) {
        stop("'na.action' must be a function")
    }
    x <- na.action(x)
    FUN(rank(x, ties.method = ties.method)/(length(x)+1))
}


