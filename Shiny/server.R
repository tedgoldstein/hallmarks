library(dplyr)
library(pracma)

options(shiny.maxRequestSize=50*1024^2) 

urlMap = list(
    "PubMed"= "https://www.ncbi.nlm.nih.gov/pubmed/",
    "ImmPort.Study.ID"= "http://www.immport.org/immport-open/public/study/study/displayStudyDetail/",
    "Strain"= "http://www.findmice.org/summary?query=",
    "Type"= "https://portal.gdc.cancer.gov/projects/",
    "Experiment.ID"= "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",
    "BioSample.ID"= "https://trace.ncbi.nlm.nih.gov/Traces/sra/sra.cgi?run=",
    "Repository.Accession"= "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",
    "PI"= "https://www.google.com/search?q="
)


hallmark_columns = c(
    "Evading_growth_suppressors",
    "Evading_immune_destruction",
    "Genome_instability",
    "Replicative_immortality",
    "Reprogramming_energy_metabolism",
    "Resisting_cell_death",
    "Sustained_angiogenesis",
    "Sustaining_proliferative_signaling",
    "Tissue_invasion_and_metastasis",
    "Tumor.promoting_inflammation")

legend_columns = c(
     "Biosample.ID",
     "Type",
     "Subtype", 
     "Species", 
     "Strain",
     "Cohort", 
     "Biosample.Name", 
     "Biosample.Description" )
# ImmPort.Study.ID	PubMed	Study.Title	PI	Biosample.ID	Experiment.ID	Cohort	Repository.Accession	Type	Subtype	Biosample.Name	Biosample.Description	Species	Strain

displayed_columns  = c(
    "Type",
    "Subtype",
    "Species",
    #"Study.Title",
    #"PI",
    #"ImmPort.Study.ID",
    #"PubMed",
    "Experiment.ID",
    "Cohort",
    "Biosample.ID",
    #"Repository.Accession",
    "Biosample.Name",
    #"Biosample.Description",
    "Strain",
    "Evading_growth_suppressors",
    "Evading_immune_destruction",
    "Genome_instability",
    "Replicative_immortality",
    "Reprogramming_energy_metabolism",
    "Resisting_cell_death",
    "Sustained_angiogenesis",
    "Sustaining_proliferative_signaling",
    "Tissue_invasion_and_metastasis",
    "Tumor.promoting_inflammation")



maxS = 3.0
minS = -3.0
diffS = maxS - minS;

rescale= function(a) {
  b = scale(a, center = T, scale = T);
  c = diffS/(max(b)-min(b))*(b-min(b))+minS
  return(c);
}


computeSignatureScore = function(X, tissue) {
    index <- Signatures$index[[tissue]];
    signaturesForTissue <- Signatures$signatures[index];

    possible = row.names(X)
    X <- apply(X, 2, rescale)
    row.names(X) <- possible
    X <- data.frame(X)
    scores = data.frame()
    
    n = length(signaturesForTissue)
    signature <- NULL
 
    for (i in 1:n) {
        # Increment the progress bar, and update the detail text.
        # incProgress(1/n, detail = paste("Doing part", i, "of", n))

        signature    <- signaturesForTissue[[i]];
        hallmark <- signature$hallmark;
        
        should  <- names(signature$w)
        genes    <- intersect(should, possible)

        printf("should=%d possible=%d actual=%d\n", length(should),length(possible),length(genes));

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
        scores = rbind(scores, score);
    }


    scores = t(scores)
    rownames(scores) = colnames(X);
    colnames(scores) = unlist(lapply(signaturesForTissue,function(sig) sig$hallmark))
    scores = scores[,1:10]

    n = nrow(scores)
    scores = cbind(scores, 
        data.frame(
            BioSample.ID = colnames(X),

            Type = rep( simpleCap(signature$cancer), n),
            Subtype = rep( simpleCap(signature$tissue), n),
            Species = rep( "none", n),
            Study.Title = rep( "none", n),
            PI = rep( "User", n),
            ImmPort.Study.ID = rep( "REF", n),
            PubMed = rep( "none", n),
            Experiment.ID = rep( "none", n),
            Cohort = rep( "none", n),
            Repository.Accession = rep( "none", n),
            Biosample.Name = rep( "none", n),
            Biosample.Description = rep( "none", n),
            Strain = rep( "none", n)));

    return (scores)
}




spaceFix <- function (x) gsub("[._]", " ", x)
TCGA = SamplesDB[SamplesDB$PI == "TCGA", ]

function(input, output, session) {
  UserState <- reactiveValues();

  DB = reactive({
    title = StudyDB[input$study,]$Study.Title
    db = SamplesDB[SamplesDB$Study.Title == title, ]
    # TCGA reference samples should be at the beginning
    type = db[1,"Type"]
    ref = TCGA[TCGA$Subtype == type,]
    db = rbind(ref, db)

    if (! is.null(UserState$uploadedScored)) {
        user = UserState$uploadedScored

        # rbind adds a digit 1 onto the end of conflicting names, I like this better.
        while (length(intersect(rownames(user), rownames(db))) > 0) {
            nms = rownames(user)
            conflict = intersect(nms, rownames(db))
            whichConflict = match(conflict, nms)
            nms[whichConflict] = lapply(nms[whichConflict], function(x) paste0(x, ".user"))
            rownames(user) = nms
        }
        
        db = rbind(user, db)
    }
    db
  })

  output$DB <- DT::renderDataTable( {
    printf("setdiff(colnames(displayed_columns),SamplesDB)=%s\n", setdiff(displayed_columns,colnames(SamplesDB)))
    db = SamplesDB[,displayed_columns]
    c = colnames(db)
    ff = lapply(c, function(colName) {
        col= db[,colName]
        if (colName == "Strain") {
            encoded = gsub(" .*", "", col)
            encoded = url_encode(encoded)
        } else
            encoded = col

        if (colName %in% names(urlMap)) {
            url = urlMap[colName]
            sprintf("<a href='%s%s'  target='OMFS-aux' >%s</a>", url, encoded, col)
        } else
            col
    })
    db  = data.frame( ff )
    c = lapply(c, spaceFix)
    DT::datatable(db, colnames=c, 
            selection = list(selected = c(1, 2)), escape=FALSE 
    )
  } )

  output$radarchart <- renderRadarChart({
    s = input$DB_rows_selected

    hdb = SamplesDB[s, hallmark_columns]
    ldb = SamplesDB[s, legend_columns]
    if (nrow(ldb) > 0)
        legend =  apply(ldb, 1, function(x) paste(x, collapse=" "))
    else
        legend = "none selected"

    list(
      nrow = nrow(hdb),
      rownames = rownames(hdb),
      colnames = colnames(hdb),
      df = hdb,
      zodiac = TRUE, # input$zodiac,
      legend = legend
    )
  })

  observeEvent( input$file1,  {
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)


    d = read.csv(inFile$datapath, as.is=TRUE, header = TRUE, sep = "\t")
    
    validate(
        # need to handle case where there are no column labels
        need(nrow(d) > 50, "Insufficient data. Need thousands of genes"),
        need(ncol(d) > 2, "Insufficient data. First column should be genes, second column should be gene expression"),
        need(class(d[,1]) == "character", "Type of first column must be numeric")
    )
    cn = colnames(d)
    cn[1] = "gene_id"
    colnames(d) = cn

    if ( any(d > 1000) ) {
        d = d %>% group_by(gene_id) %>% summarise_all(funs(sum))
        e = "Aggregating duplicate rows by summing counts"
    } else {
        d = d %>% group_by(gene_id) %>% summarise_all(funs(mean))
        e = "Aggregating duplicate rows by averaging"
    }


    cat("\nbefore\n")
    d = as.data.frame(d)
    cat("\nafter\n")
    rownames(d) <- d[,1]
    d[,1] <- NULL

    UserState$uploaded = d
    output$Uploaded <- DT::renderDataTable( { DT::datatable(UserState$uploaded, options = list( pageLength = 5)) })
  })

    output$Scored <- DT::renderDataTable( { 
        if (! is.null(UserState$uploaded)) {
            UserState$uploadedScored = computeSignatureScore(UserState$uploaded, input$tissue)
            DT::datatable(UserState$uploadedScored)
        }
    })


} # end of server.R singletonfunction

