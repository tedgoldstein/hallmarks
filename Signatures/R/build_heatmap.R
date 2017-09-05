# make a heatmap from a scores file. useful for diagnostics
library(gplots)

args = commandArgs(trailing=T);

hmap.col <- bluered(15)
hmap.breaks <- c(
-200, -100,0,
100,200,300,400,500,
600,700,800,900,1000,
1100,1200,1500);


for ( fn in args) {
    cat(fn);
    X = read.table(fn, header=T, row.names=1, sep="\t")
    X = data.matrix(X)
    fn = gsub(".txt", "", fn);

    pdf(paste0(fn,".pdf"), width=11)

    rn = row.names(X)
    rn = gsub("HALLMARK_", "", rn)
    row.names(X) = rn

    heatmap.2(X,trace="none", col=hmap.col, dendrogram = "none", breaks=hmap.breaks, key=FALSE,
      # rowv=F, colv=F,
      # lmat=rbind(c(2),c(3),c(1),c(4)), 
      # lhei=c(1,1,9,0), lwid=c(2),
      lwid=c(0.1,4), lhei=c(0.1,4),
      margins = c(8, 18)
    )
    dev.off()
}

quit(save = "no", status = 0, runLast = TRUE)



library(RJSONIO)


## heatmap-sig.R - tools for plotting heatmaps and signatures
##
## by Artem Sokolov

## Applies a signature to a dataset and plots the signature scores,
##   the signature weights, and the associated genes as a heatmap
## Inputs:
##   X - p-by-n matrix of n samples in p dimensional space
##   y - n-by-1 vector of labels
##   w - k-by-1 signature of k genes
##   b - scalar, the signature bias term
##   vGenes - a vector of gene names to plot in the heatmap
##	(Note: does not affect the score computation; all genes in w are used for that)
heatmap.sig <- function( X, y, w, b, vGenes = names(w) )
{
    ## Argument verification
    stopifnot( all( colnames(X) == names(y) ) )
    stopifnot( is.null( dim(w) ) )	## w must be a vector, not a matrix
    stopifnot( all( names(w) %in% rownames(X) ) )

    ## Reduce the data to the signature space and compute the scores
    X <- X[names(w),]
    s <- drop( t(X) %*% w + b )

    ## Scale the values to be between 0 and 1
    s <- s - min(s)
    s <- s / max(s)

    ## Order the data hi -> lo by the signature score
    j <- order( s, decreasing=TRUE )
    X <- X[,j]; y <- y[j]; s <- s[j]

    ## Score computation is done; now reduce to vGenes for plotting
    X <- X[vGenes,]
    w <- w[vGenes]
    
    ## Order the rows by the signature weights
    j <- order( w, decreasing=TRUE )
    X <- X[j,]; w <- w[j]

    ## Median-center the genes
    m <- apply( X, 1, median )
    X <- X - m

    ## Don't load the library until we actually need it
    library( gplots )

    ## The function plots the signature scores as a waterfall plot,
    ##   the signature weights, and some auxiliary elements
    f.sig <- function()
      {
        ## Layout Area 6: Plot the waterfall
        par( mar = c( 1, 0.25, 1, 9.25 ), mgp = c( 3, 2, 1) )
#        wf.col <- c( "red", "blue" )[ as.integer(s>0) + 1 ]
        mypos <- barplot( s, col="darkgray", yaxs="r", xaxs="i", axes=FALSE,
                         names.arg = FALSE, cex.axis = 1.5 )
        axis( 4 ); mtext( "Signature Score", 4, 5, cex=1.5 )

        ## Layout Area 7: Plot the signature weights
        w <- rev(w)	## barplot() plots bottom to top, reverse w to adjust
        par( mar = c( 9, 1, 0, 1 ), mgp = c( 3, 2, 1) )
        w.col <- c( "#FF8080", "#8080FF" )[(sign(w)/2 + 1.5)]
        w.mbs <- max( abs(w) )
        mypos <- barplot( w, horiz = TRUE, col=w.col, yaxs="i", xaxs="r", cex.axis = 1.5,
                         xlab="", border = NA, space=0, xlim=c(-w.mbs,w.mbs) )
        mtext( "Gene Weight", 1, 5, cex=1.5 )

        ## Display the gene labels and weights
        text( -0.05*w.mbs, mypos, names(w), pos=2, cex=2 )
        text( 0.05*w.mbs, mypos, format(round( w, 4 )), pos=4, cex=2 )
        
        ## Layout Area 8: Plot the "True Labels ->" element
        par( mar = c(0,0,0,0), font=2 )
        plot( 0, type="n", xlim = c( -10, 2 ), ylim = c( -1.25, 1), axes=FALSE )
        arrows( 0, 0, 1, 0, length=0.05, lwd=2 )
        text( 0, 0, "True Labels", pos=2, cex=2.5 )
      }

    ## Layout of the plot elements:
    ## 1 - The true label colors
    ## 2 - The heatmap
    ## 3 - The dendrogram of the row (gene) clustering (not plotted)
    ## 4 - The dendrogram of the column (sample) clustering (not plotted)
    ## 5 - The color key
    ## 6 - (Used by f.sig) the waterfall plot of signature scores
    ## 7 - (Used by f.sig) the barplot of signature weights
    ## 8 - (Used by f.sig) the "Pathology Calls ->" element
    lmat <- matrix( c(5,6,3, 8,1,3, 7,2,3, 4,4,4), 4, 3, byrow=TRUE )
    lhei <- c( 1.5, 0.2, 6, 0.001 )	## Due to margin restrictions, layout area 4 is "squished" at the bottom
    lwid <- c( 1.5, 4, 0.001 )		## Due to margin restrictions, layout area 3 is "squished" on the right

    ## Map the labels to colors
    z <- (y - min(y)) / (max(y) - min(y))
    lbl.col <- rgb( 1-z, 0, z )
    hmap.col <- bluered(49)
    
    ## Plot the heatmap and things
    heatmap.2( X, trace="none", col=hmap.col, cexRow=1.5, cexCol=1.3,
              Colv = FALSE, Rowv = FALSE, dendrogram = "none",
              ColSideColors = lbl.col, adjCol=c(NA,0.5),
              offsetCol=0, offsetRow=0, margins=c(7,7),
              key.title = NA, key.ylab = NA, key.xlab = NA,
              lmat = lmat, lhei = lhei, lwid = lwid, extrafun = f.sig )
}

args <- commandArgs(trailingOnly = TRUE)

for (i in 1:length(args)){
    model <- fromJSON(args[i]);
    Xf = paste0("../tmp/", model$tissue,".GTEX.TCGA.", model$hallmark, ".data");
    yf = paste0("../tmp/", model$tissue,".GTEX.TCGA.", model$hallmark, ".phen");
    pf = paste0(model$tissue,".GTEX.TCGA.", model$hallmark, ".pdf");

    X <- t(read.table(Xf, header=TRUE, row.names=1));
    y <- as.vector(scan(yf))

    pdf(pf)
    heatmap.sig(t(X),y, model$w, model$b)
    dev.off()
}


