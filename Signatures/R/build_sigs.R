library(gelnet)
library(RJSONIO)

# gelNet.R input_data input_dichotomy input_network output_model

args <- commandArgs(trailingOnly = TRUE)
cat(args)
cat("\n")

#load up parameters, X, y, A
#   arg[1] X is the data
#   arg[2] y is the phenotype states
#   arg[3] A is the Adjacency matric
#   arg[4] output_model is the name of the output file
#   arg[5] hallmark is the hallmark annotation
#   arg[6] tisuse is the tissue annotation

X <- t(read.table(args[1], header=TRUE, row.names=1));
y <- as.vector(scan(args[2]))
y <- factor( y > 0, levels=c(TRUE,FALSE) )
A <- read.table(args[3], header=TRUE, row.names=1);
output_model = args[4]
hallmark = args[5]
tissue = args[6]
genes <- row.names(A)
L <- adj2nlapl(A)
X <- X[,genes]

# run the regression
model <- gelnet( X, y, 0.1, 1, P=L, balanced=TRUE)

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

# save
write(toJSON(model), file=args[4])
