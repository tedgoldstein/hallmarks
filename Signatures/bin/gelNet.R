library(gelnet)
library(RJSONIO)

# gelNet.R input_data input_dichotomy input_network output_model



rescale= function(b) {
  maxb = max(b)
  minb = min(b)
  cat("\nmaxb=")
  cat(maxb);
  cat("\nminb=")
  cat(minb);
  cat("\n\n")
  c = 6/(maxb-minb)*(b-minb)-3
  d = scale(c, center = T, scale = T);
  return(d);
}



args <- commandArgs(trailingOnly = TRUE)
cat(args)
cat("\n")

#load up parameters, X, y, A
#   arg[1] X is the data
#   arg[2] y is the phenotype states
#   arg[3] A is the Adjacency matric
#   arg[4] output_model is the name of the output file
#   arg[5] hallmark is the hallmark annotation
#   arg[6] is the healthy tissue annotation
#   arg[7] is the cancer tissue annotation

cat(args[1]);
X <- read.table(args[1], header=TRUE, row.names=1)
X <- rescale(X)
X <- t(X);
cat(args[2]);
y <- as.vector(scan(args[2]))
yy <- factor( y > 0, levels=c(TRUE,FALSE) )
cat(args[3]);
A <- read.table(args[3], header=TRUE, row.names=1);
output_model = args[4]
hallmark = args[5]
tissue = args[6]
cancer = args[7]
genes <- row.names(A)
L <- adj2nlapl(A)
X <- X[,genes]

# run the regression
model <- gelnet( X, yy, 0.1, 1, P=L, balanced=TRUE)

# clean up and remove zero weights
nonzero <- which(model$w != 0)
weights <- model$w[nonzero]
weight_names <- names(model$w)[nonzero]
names(weights) <- weight_names

model$name = output_model;
model$w = weights;
model$numberOfGenes = length(weights);
model$hallmark = hallmark;
model$tissue = tissue;
model$cancer = cancer;

genes = labels(model$w);
XX = X[,genes];
samples=gsub("\\.", "-", row.names(X));
if (length(model$w) == 1) {
  raw= XX * model$w + model$b;
} else {
  raw= XX %*% model$w + model$b;
}

reference = data.frame( samples=samples, rawscore=raw,  labels= y);
reference = with(reference, reference[order(rawscore),]);

model$reference = reference

# save
write(toJSON(model), file=args[4])

