# Run linear equations with coeficients specified in 
# expects there to be a file in current directory named signtures,  json file producted by
# load and unpack the signatures for this specific tissue (specified in args[1]) 
# read expression data in file  in argargs[[2]]
# write output to  file=args[[3]

library(RJSONIO)

args <- commandArgs(trailingOnly = TRUE)
# args = c("colon_adenocarcinoma", "tpm_matrix_colon_superOrtho_meta.tsv.log_norm", "/tmp/colon.txt")

printf <- function(...) cat(sprintf(...))


# load and unpack the signatures for this specific tissue (specified in args[1])
signatures <- fromJSON("../Signatures/signatures");
tissue <- gsub(" ", ".", args[1]);
index <- signatures$index[[tissue]];
signatures <- signatures$signatures[index];

maxS = 3.0
minS = -3.0
diffS = maxS - minS;

rescale= function(a) {
  b = scale(a, center = T, scale = T);
  c = diffS/(max(b)-min(b))*(b-min(b))+minS
  return(c);
}

reference = list();

process = function() {

    # load the data
    X <- read.table(args[[2]], header=TRUE, row.names=1)
    possible = row.names(X)
    X <- apply(X, 2, rescale)
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
        
        cat(hallmark)
        cat("\n")
        should  <- names(signature$w)
        genes    <- intersect(should, possible)
        printf("should=%d possible=%d actual=%d\n", length(should),length(possible),length(genes));
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

# write.table(scores, sep="\t", file=args[[3]], quote=F, row.names = T)

out = file(paste0(args[[3]],".txt"), open="w");
write.table(scores.colnames, col.names=F, file=out, sep="\t", quote=FALSE);
write.table(scores, col.names=F, file=out, sep="\t", quote=FALSE);
close(out)
