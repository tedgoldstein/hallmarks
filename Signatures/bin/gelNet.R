library(data.table)
library(gelnet)
library(RJSONIO)
library(Matrix)
library(igraph)
library(dplyr)

library(qusage)

# gelNet.R tissue cancer



# LOAD UP THE DATASETS
if (!exists("need")) {
    need = readLines("../GeneLists/genes.all")
}

if (!exists("NETWORK")) {
    NETWORK = unique(read.table("ReferenceData/NETWORK", as.is=TRUE)[,-2])
    NETWORK = NETWORK %>% filter(V1 %in% need) %>% filter(V3 %in% need)
}

if (!exists("NormalizedReferenceData")) {
  # NormalizedReferenceData = fread("ReferenceData/NormalizedReferenceData.txt", sep="\t")
  # rn = unlist(NormalizedReferenceData [,1])
  # NormalizedReferenceData = as.data.table(NormalizedReferenceData[,-1])
  # rownames(NormalizedReferenceData) = rn
  load("ReferenceData/NormalizedReferenceData.rda")
  m = na.omit(NormalizedReferenceData)
  NormalizedReferenceData =  as.data.frame(m)
  rownames(NormalizedReferenceData) = rownames(m)
  rm(m)
}

if (!exists("hallmark.genes")) {
    hallmark.genes = read.gmt("../GeneLists/genes.gmt")
}

graphFor = function(genes)  {
  subnet = NETWORK %>% filter(V1 %in% genes) %>% filter(V3 %in% genes)
  graph = as.undirected(graph.edgelist(  as.matrix(subnet) ))
  m = as.matrix(get.adjacency(graph))
  m
}

foreachHallmark = function(hallmark, X, y, yy, tissue, cancer) {

    # filter to this hallmarks's genes
    X = na.omit(X)
    genes = rownames(X)
    genes = intersect(genes,  hallmark.genes[[hallmark]])
    X = X[genes,]

    A = graphFor(genes)

    # further restrict to the genes in the graph
    X = X[rownames(A),]
    
    L <- adj2nlapl(A)

    # run the regression
    model <- gelnet( t(X), y, 0.1, 1, P=L, balanced=TRUE )

    # clean up and remove zero weights
    nonzero <- which(model$w != 0)
    weights <- model$w[nonzero]
    weight_names <- names(model$w)[nonzero]
    names(weights) <- weight_names

    output_model = paste0( "Models/", tissue, ".to.", cancer, ".", hallmark, ".signature")
    model$name = output_model;
    model$w = weights;
    model$numberOfGenes = length(weights);
    model$hallmark = hallmark;
    model$tissue = tissue;
    model$cancer = cancer;

    modelGenes = labels(model$w);
    testX = t(as.matrix(X[modelGenes,]))
 
    raw = testX %*% model$w + model$b;
    sp = sign(raw) * yy
    accuracy = sum(sp>0) / length(raw)
    
    samples=gsub("\\.", "-", colnames(X));
    reference = data.frame( samples=samples, rawscore=raw,  labels= y);
    reference = with(reference, reference[order(reference$rawscore),]);

    model$reference = reference
    model$accuracy = accuracy

    # save
    write(toJSON(model), file=output_model)
}

foreachCancer = function(tissue, cancer, X)  {
    n = paste0("lists/normal.", tissue, ".",  cancer)
    t = paste0("lists/tumor.",  tissue, ".",  cancer)
    samples = colnames(X)

    normalSamples = intersect(samples, readLines( n ))
    tumorSamples = intersect(samples, readLines( t ))

    nt = c(normalSamples, tumorSamples)
    X = X[, nt]
    y = factor(c(rep(TRUE,length(normalSamples)), rep(FALSE,length(tumorSamples))))
    yy = c(rep(-1,length(normalSamples)), rep(1,length(tumorSamples)))
    names(y) = nt
    names(yy) = nt

    hallmark = "Evading_growth_suppressors"
    # foreachHallmark(hallmark, X, y, yy, tissue, cancer)
    lapply( names(hallmark.genes), function(hallmark) foreachHallmark(hallmark, X, y, yy, tissue, cancer))
}

# foreachCancer("Colon", "colon_adenocarcinoma", NormalizedReferenceData) 

print(system.time(
  lapply(list.files(path = "lists", pattern = "tumor.*" ), function(fn) {
      fn = unlist(strsplit(x=fn,split = "[.]"))
      tissue = fn[2]
      cancer = fn[3]
      print(cancer)
      foreachCancer(tissue, cancer, NormalizedReferenceData) 
  })
))
