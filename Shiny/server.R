library(dplyr)
library(pracma)
library(png)
options(shiny.maxRequestSize=50*1024^2) 

urlMap = list(
    "PubMed"= "https://www.ncbi.nlm.nih.gov/pubmed/",
    "ImmPort.Study.ID"= "http://www.immport.org/immport-open/public/study/study/displayStudyDetail/",
    "Strain"= "http://www.findmice.org/summary?query=",
    "Type"= "https://portal.gdc.cancer.gov/projects/",
    "Experiment_ID"= "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",
    "Biosample_ID"= "https://trace.ncbi.nlm.nih.gov/Traces/sra/sra.cgi?run=",
    "Repository_Accession"= "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",
    "PI"= "https://www.google.com/search?q="
)

mgrep = function(l, x)
   sort(unique(unlist(lapply(unlist(l), 
        function(pat)  {
            g = grep(pat, x)
            g
        }
    ))))


hallmark_columns = c(
    "Evading_growth_suppressors",
    "Sustaining_proliferative_signaling",
    "Reprogramming_energy_metabolism",
    "Resisting_cell_death",
    "Genome_instability",
    "Sustained_angiogenesis",
    "Tissue_invasion_and_metastasis",
    "Tumor_promoting_inflammation",
    "Replicative_immortality",
    "Evading_immune_destruction")

legend_columns_default = c(
     "Hallmark",
     "Cancer_Type",
     "Repository_Accession"
     #"Biosample_ID",
     #"Type",
     #"Subtype", 
     #"Species", 
     #"Strain",
     #"Cohort", 
     #"Biosample_Name", 
     #"Biosample_Description"
)

#legend_columns = c(legend_columns_default, input$SelectLegendColumn)

# ImmPort.Study.ID	PubMed	Study.Title	PI	Biosample.ID	Experiment.ID	Cohort	Repository.Accession	Type	Subtype	Biosample.Name	Biosample.Description	Species	Strain

displayed_columns  = c(
    "Hallmark",
    "Biosample_ID",
    #"Biosample_Description",
    #"Type",
    "Subtype",
    "Species",
    #"Study_Title",
    #"PI",
    #"ImmPort_Study_ID",
    "PubMed",
    "Experiment_ID",
    "Cohort",
    "Repository_Accession",
    #"Biosample_Name",
    "Strain",
    "Cancer_Type",
    "Cancer_Model",
    "Tissue",
    "Cell_Type",
    "Cell_Line",
    "Treatment",
    "Evading_growth_suppressors",
    "Evading_immune_destruction",
    "Genome_instability",
    "Replicative_immortality",
    "Reprogramming_energy_metabolism",
    "Resisting_cell_death",
    "Sustained_angiogenesis",
    "Sustaining_proliferative_signaling",
    "Tissue_invasion_and_metastasis",
    "Tumor_promoting_inflammation")

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




computeSignatureScore = function(X, cancer) {
    signaturesForTissue <- Filter(function(ss) ss$cancer == cancer, Signatures$signatures)

    possible = as.character(row.names(X))
    X = apply(X, 2, function(x) scale(rank.normalize(x), scale=TRUE, center=TRUE))

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
        genes    <- as.character(intersect(should, possible))

        # printf("should=%d possible=%d actual=%d\n", length(should),length(possible),length(genes));

        score = data.frame();
        posScale <- signature$posScale;
        negScale <- signature$negScale;
        w = signature$w[genes]
    
        XX <- t(X[genes,])
        #cat(XX);
      
    
    
        raw = -XX %*% w + signature$b;
        #heat= XX * w + signature$b;
    
        for (j in 1:length(raw)) {
            value = raw[j];
            if (value < 0) {
                score[1,j] = round(500  - (negScale * raw[j]));
            } else {
                score[1,j] = round( (posScale * raw[j]) + 500);
            }
            score[1,j] = max(score[1,j], 1) # never less than one
        }
        scores = rbind(scores, score);
    }


    scores = t(scores)
    rownames(scores) = colnames(X);
    colnames(scores) = unlist(lapply(signaturesForTissue,function(sig) sig$hallmark))
    scores = scores[,1:10]

    n = nrow(scores)
    # The Hallmark is the geometric mean.  Note, all scores must be greater than zero.
    Hallmark = apply(scores, 1, function(x)  round(exp(mean(log(x)))))
    df =  data.frame(
	Repository_Accession = colnames(X),
	Hallmark = Hallmark,
	Biosample_ID = colnames(X),
	Cancer_Type = rep( simpleCap(signature$cancer), n), 
	Cancer_Model = rep( simpleCap(signature$cancer), n),
	Type = rep( simpleCap(signature$cancer), n),
	Subtype = rep( simpleCap(signature$tissue), n),
	Species = rep( "none", n),
	Study_Title = rep( "none", n),
	PI = rep( "User", n),
	ImmPort_Study_ID = rep( "REF", n),
	PubMed = rep( "none", n),
	Experiment_ID = rep( "none", n),
	Cohort = rep( "none", n),
	Biosample_Name = rep( "none", n),
	Biosample_Description = rep( "none", n),
	Tissue = rep( "none", n),
        Cell_Type = rep( "none", n),
        Cell_Line = rep( "none", n),
        Treatment = rep( "none", n),
	Strain = rep( "none", n), stringsAsFactors=FALSE
    );

    scores = cbind(scores, df)
    scores = scores[,c("Repository_Accession", "Hallmark", "Evading_growth_suppressors", "Evading_immune_destruction", "Genome_instability", "Replicative_immortality", "Reprogramming_energy_metabolism", "Resisting_cell_death", "Sustained_angiogenesis", "Sustaining_proliferative_signaling", "Tissue_invasion_and_metastasis", "Tumor_promoting_inflammation", "Cancer_Model", "ImmPort_Study_ID", "PubMed", "Study_Title", "PI", "Biosample_ID", "Experiment_ID", "Cohort", "Type", "Subtype", "Biosample_Name", "Biosample_Description", "Species", "Strain", "Cancer_Type", "Tissue", "Cell_Type", "Cell_Line","Treatment")]

    return (scores)
}


geometric_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}


spaceFix <- function (x) gsub("[._]", " ", x)


function(input, output, session) {

  UserState <- reactiveValues();
  
  UserState$studies_selected <- 1
  UserState$studies <- StudiesDB[1, "ImmPort_Study_ID"]
  db2 =SamplesDB[ mgrep(isolate(UserState$studies), SamplesDB$ImmPort_Study_ID), ] 
  UserState$DB = db2
  sel = c(which.min(db2$Hallmark), which.max(db2$Hallmark))
  UserState$samples_selected = sel
  UserState$samples = rownames(db2)[sel]
 
  initSamples = function() {
    db = SamplesDB[ mgrep(isolate(UserState$studies), SamplesDB$ImmPort_Study_ID), ]
    UserState$DB = db
    UserState$samples_selected = 0
    UserState$samples = rownames(db)[0]
  }
  #initSamples()

  setBookmarkExclude(c(
        "Cancer",
        # want this "study",
        "file1", 
        "Uploaded", 
        "DB_cell_clicked",
	"clearStudies",
	"clearSamples",
        "DB_rows_all",
        "DB_rows_current",
        "DB_rows_selected",
	"DB_row_last_clicked",
	"study_row_last_clicked",
        "DB_search",
        "DB_state",
        "Scored_cell_clicked",
        "Scored_rows_all",
        "Scored_rows_current",
        "Scored_rows_selected",
        "Scored_search",
        "Scored_state",
        "study_cell_clicked",
        "study_rows_all",
        "study_rows_current",
        "study_rows_selected",
        "study_search",
        "study_state",
        "Uploaded_cell_clicked",
        "Uploaded_rows_all"))

   
   
  add.alpha <- function(col, alpha=0.5){
      apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha))  
  }

  radar_colors= add.alpha(rainbow(1000))


  rgba = function(x) { 
      y = col2rgb(x)
      paste("rgba(", paste(unname(y), collapse=","), ",0.5)", sep="")
  }
  
  transformURL = function(db) {
    ff = lapply(colnames(db), function(colName) {
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

    ff = as.data.frame(ff)
    colnames(ff) = colnames(db)
    ff
  }



  output$DB <- DT::renderDataTable( {
    db = UserState$DB[ , displayed_columns ]
    db = transformURL(db)
    DT::datatable(db, selection = list(selected = as.list(UserState$samples_selected)), escape=FALSE  )
  })

 zodiac = readPNG("Zodiac800.png")

 output$radarImage <- renderImage({
    # Read plot2's width and height. These are reactive values, so this
    # expression will re-run whenever these values change.
    width  <- 800 # session$clientData$output_plot2_width
    height <- 800 # session$clientData$output_plot2_height

    # A temp file to save the output.
    outfile <- tempfile(fileext='.png')

    png(outfile, width=width, height=height)

    plotRadarChart(TRUE)
    dev.off()

    # Return a list containing the filename
    list(src = outfile,
         width = width,
         height = height,
         alt = "Please select Studies or Upload data to see the visualization")
  }, deleteFile = TRUE)



  output$Legend = renderUI( {
    db = UserState$DB[unlist(UserState$samples),]
    if (!is.null(db)) {
      legend_columns = c(legend_columns_default, input$SelectLgColumn)
      ldb = db[, legend_columns]
      ldb = ldb[order(ldb$Repository_Accession),]
  
      if (nrow(ldb) > 0)
          # legend =  apply(ldb, 1, function(x) paste(x, collapse=" "))
          legend =  apply(ldb, 1, function(x) {
              paste(x["Cancer_Type"], x["Repository_Accession"], x[input$SelectLgColumn])
          })
      else
          legend = list("none selected")
      
     wrapDiv = function(i)  {
       hallmark = ldb[i, "Hallmark"]
       hallmark_color = rgba(radar_colors[hallmark])
       style =  paste("width: 30px; height: 25; border:1px solid #000; background-color: ", hallmark_color,
              ";  display: inline-block; vertical-align: top; margin: 5px; font-weight:bold;")
      
       tags$li( tags$span( tags$span(class="text-center", style=style, tags$em(hallmark)),tags$span(style="text-align: left;",  legend[i])))
     }
      
      tags$ul(style="list-style: none;", lapply(1:length(legend), wrapDiv))
    }
  })



  plotRadarChart = function(zodiacLayout) {
    data = UserState$DB[ unlist(UserState$samples), hallmark_columns ]
    hallmarks = UserState$DB[ unlist(UserState$samples), "Hallmark" ]
    hallmark_colors = radar_colors[hallmarks]
    if (!is.null(data)) {
  
      # Add max and min of each topic to show on the plot!
      data=rbind(rep(1000,5) , rep(0,5) , data)
  
      colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) ,     rgb(0.7,0.5,0.1,0.9) )
      colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
      if (zodiacLayout) {
          image=zodiac
          rotate=-18.0
          scale=0.66
          no_vlabels=TRUE
      } else {
          image=NULL
          rotate=0
          scale=1.0
          no_vlabels=FALSE
      }
  
      if (nrow(data) > 2)
        radarchart( data  , axistype=1 ,  no_vlabels=no_vlabels,
            image=image, rotate=rotate, scale=scale, 
            caxislabels=c(0,250,500, 750,1000),
            #custom polygon
            pcol=hallmark_colors , plwd=4 , plty=1,
    
            #custom the grid
            cglcol="grey", cglty=2, axislabcol="grey", cglwd=0.8,
    
            #custom labels
            vlcex=0.8 
        )
    }
  }

  Mapgene  = read.table("geneSymbol_to_geneID.txt", header = TRUE, sep = "\t")
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
        need(nrow(d) > 100, "Insufficient data. Need thousands of genes"),
        need(ncol(d) > 2, "Insufficient data. First column should be genes, second column should be gene expression"),
        need(class(d[,1]) == "character", "Type of first column must be numeric")
    )
    cn = colnames(d)
    cn[1] = "gene_id"
    colnames(d) = cn

    #convert gene Symbol to geneID
    genename <- as.character(d[,1])
    GeneID <- as.character(with(Mapgene, geneID[match(genename,gene)]))
    d <- cbind(GeneID,d)
    #remove non-value Gene ID
    d <- subset(d, GeneID != "NA")
    d <- subset(d, select = -c(gene_id))

    if ( any(d[2:nrow(d),2:ncol(d)] > 1000) ) {
      #it appears to be raw counts
        d = d %>% group_by(GeneID) %>% summarise_all(funs(sum))
        e = "Aggregating duplicate rows by summing counts"
    } else if ( all(d[2:nrow(d),2:ncol(d)] >= 0 & d[2:nrow(d),2:ncol(d)] <= 20) ) {
      #it appears to be normalized log in some way
        d = d %>% group_by(GeneID) %>% summarise_all(funs(geometric_mean))
        e = "Aggregating duplicate rows by geometric mean averaging"

    } else {
        d = d %>% group_by(GeneID) %>% summarise_all(funs(mean))
        e = "Aggregating duplicate rows by averaging"
    }


    d = as.data.frame(d)
    rownames(d) <- as.character(d[,1])
    d[,1] <- NULL

    UserState$uploaded = d
    output$Uploaded <- DT::renderDataTable( { DT::datatable(UserState$uploaded, options = list( pageLength = 10), colnames = c('geneID' = 1)) })
  })

  StudiesDB <- StudiesDB[order(StudiesDB$Cancer_Type),]
  output$study <- DT::renderDataTable( { 
        DT::datatable(StudiesDB, selection = list(selected = as.list(UserState$studies_selected)))
    })

    output$Scored <- DT::renderDataTable( { 
        if (! is.null(UserState$uploaded)) {
            UserState$uploadedScored = computeSignatureScore(UserState$uploaded, input$Cancer)
            DT::datatable(UserState$uploadedScored, rownames=FALSE)
        }
    })


   observe({
        db = SamplesDB[ mgrep(UserState$studies, SamplesDB$ImmPort_Study_ID), ]
        if (! is.null(UserState$uploadedScored)) {
            user = UserState$uploadedScored
            db = rbind(user, db)
            #rownames(db) = db$Repository_Accession
	    rownames(db) = paste(db$Repository_Accession, db$Cancer_Model, sep=".")
        }
        UserState$DB = db
   })


   output$downloadSamples <- downloadHandler(
        filename = function() {
          paste("OMFS.csv", sep = "")
        },
        content = function(file) {
          write.csv(DB(), file, row.names = FALSE)
        }
   )

  onRestored(function(state) {
    UserState$studies <<- as.vector(state$values$studies)
    UserState$studies_selected <- as.vector(mgrep(UserState$studies, StudiesDB$ImmPort_Study_ID))
    # UserState$DB = SamplesDB[ mgrep(UserState$studies, SamplesDB$ImmPort.Study.ID), ]
    initSamples()
    

    samples <- strsplit(state$values$samples,",")
    UserState$samples = as.vector(samples)
    UserState$samples_selected = as.vector(unlist(mgrep(samples, UserState$DB$Repository_Accession)))

  })
  
  onBookmarked(function(url) {
        updateQueryString(url)
  })


  onBookmark(function(state) {
    # state$values$savedTime <- Sys.time()
    state$values$studies <- UserState$studies
    state$values$samples <- paste(UserState$samples, collapse=",")
  })

   observeEvent(input$study_cell_clicked, { 
     UserState$studies = StudiesDB[input$study_rows_selected,  "ImmPort_Study_ID"]
     UserState$DB = SamplesDB[ mgrep(UserState$studies, SamplesDB$ImmPort_Study_ID), ]
     initSamples()
     
     session$doBookmark()
   })

   observeEvent(input$DB_cell_clicked, { 
     UserState$samples <<- UserState$DB[input$DB_rows_selected, "Repository_Accession"]
     session$doBookmark()
   })

  observeEvent(input$clearSamples, {
	UserState$samples_selected <- 0
        UserState$samples <- vector(mode = "character", length = 0)
	output$DB <- DT::renderDataTable( {
		db = UserState$DB[ , displayed_columns ]
		db = transformURL(db)
		DT::datatable(db, selection = list(selected = as.list(UserState$samples_selected)), escape=FALSE)
	})
	session$doBookmark()
  })

  observeEvent(input$clearStudies, {
	UserState$studies_selected <- 0
	#UserState$studies <- vector(mode = "character", length = 0)
	StudiesDB <- StudiesDB[order(StudiesDB$Cancer_Type),]
	output$study <- DT::renderDataTable({
		DT::datatable(StudiesDB, selection = list(selected = as.list(UserState$studies_selected)))
	})
	#UserState$studies = StudiesDB[input$study_rows_selected,  "ImmPort_Study_ID"]
	#UserState$DB = SamplesDB[ mgrep(UserState$studies, SamplesDB$ImmPort_Study_ID), ]
	#initSamples()
	session$doBookmark()
  })  

} # end of server.R singletonfunction
