library(dplyr)
library(pracma)
library(png)

options(shiny.maxRequestSize=50*1024^2) 

urlMap = list(
    "PubMed"= "https://www.ncbi.nlm.nih.gov/pubmed/",
    "ImmPort.Study.ID"= "http://www.immport.org/immport-open/public/study/study/displayStudyDetail/",
    "Strain"= "http://www.findmice.org/summary?query=",
    "Type"= "https://portal.gdc.cancer.gov/projects/",
    "Experiment.ID"= "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",
    "Biosample.ID"= "https://trace.ncbi.nlm.nih.gov/Traces/sra/sra.cgi?run=",
    "Repository.Accession"= "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",
    "PI"= "https://www.google.com/search?q="
)

mgrep = function(l, x)
   unique(unlist(lapply(unlist(l), 
        function(pat)  {
            g = grep(pat, x)
            g
        }
    )))


hallmark_columns = c(
    "Evading_growth_suppressors",
    "Sustaining_proliferative_signaling",
    "Reprogramming_energy_metabolism",
    "Resisting_cell_death",
    "Genome_instability",
    "Sustained_angiogenesis",
    "Tissue_invasion_and_metastasis",
    "Tumor.promoting_inflammation",
    "Replicative_immortality",
    "Evading_immune_destruction")

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



rescale= function(a) {
  scale(a, center = TRUE, scale = TRUE);
}


computeSignatureScore = function(X, cancer) {
    signaturesForTissue <- Filter(function(ss) ss$cancer == cancer, Signatures$signatures)

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

        # printf("should=%d possible=%d actual=%d\n", length(should),length(possible),length(genes));

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
            Biosample.ID = colnames(X),
            Cancer.Type =rep( simpleCap(signature$cancer), n), 

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
            Strain = rep( "none", n), stringsAsFactors=FALSE));

    return (scores)
}




spaceFix <- function (x) gsub("[._]", " ", x)

function(input, output, session) {
  UserState <- reactiveValues();

  RestoreState <- reactiveValues();
  RestoreState$studies = list(rownames(StudiesDB)[1])
  RestoreState$samples = rownames(SamplesDB)[c(1,2)]
  
  setBookmarkExclude(c(
        # "Cancer",
        # want this "study",
        "file1", 
        "Uploaded", 
        "DB_cell_clicked",
        "DB_rows_all",
        "DB_rows_current",
        "DB_rows_selected",
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

  radar_colors= add.alpha(rainbow(8))


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
    db = UserState$DB[,displayed_columns]
    db = transformURL(db)

    sel = RestoreState$selected

    if (length(sel) == 0) {
        selected = c(1, 2)
    } else {
        selected = unlist(lapply(unlist(sel), 
            function(pat)  {
                grep(pat, db$Biosample.ID)
            }
        ))
    }
    DT::datatable(db, selection = list(selected = selected), escape=FALSE  )
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
         alt = "This is alternate text")
  }, deleteFile = TRUE)



  output$Legend = renderUI( {
    db = UserState$SelectedDB
    if (!is.null(db)) {
      ldb = db[, legend_columns]
  
      if (nrow(ldb) > 0)
          # legend =  apply(ldb, 1, function(x) paste(x, collapse=" "))
          legend =  apply(ldb, 1, function(x) {
              paste(x["Biosample.ID"], x["Biosample.Description"])
          })
      else
          legend = list("none selected")
      
     wrapDiv = function(i)  {
       style =  paste("width: 20px; height: 20px; border:1px solid #000; background-color: ",
              rgba(radar_colors[i]),
              ";  display: inline-block; vertical-align: top; margin: 5px;")
      
       tags$li( div( tags$span(style=style), tags$span(style="text-align: left;",  legend[i])))
     }
      
      tags$ul(style="list-style: none;", lapply(1:length(legend), wrapDiv))
    }
  })



  plotRadarChart = function(zodiacLayout) {
    data = UserState$SelectedDB
    if (!is.null(data)) {
      data = data[, hallmark_columns]
  
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
  
      radarchart( data  , axistype=1 ,  no_vlabels=no_vlabels,
          image=image, rotate=rotate, scale=scale, 
          caxislabels=c(0,250,500, 750,1000),
          #custom polygon
          pcol=radar_colors , plwd=4 , plty=1,
  
          #custom the grid
          cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
  
          #custom labels
          vlcex=0.8 
      )
    }
  }

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


    output$study <- DT::renderDataTable( { 
        selected = as.list(mgrep(RestoreState$studies, rownames(StudiesDB)))
        DT::datatable(StudiesDB, selection = list(selected = selected), rownames=FALSE)
    })

    output$Scored <- DT::renderDataTable( { 
        if (! is.null(UserState$uploaded)) {
            UserState$uploadedScored = computeSignatureScore(UserState$uploaded, input$Cancer)
            DT::datatable(UserState$uploadedScored)
        }
    })


   output$downloadSamples <- downloadHandler(
        filename = function() {
          paste("OMFS.csv", sep = "")
        },
        content = function(file) {
          write.csv(DB(), file, row.names = FALSE)
        }
   )


   observe({
        # Trigger this observer every time an input changes
        reactiveValuesToList(input)

        # phase 1 stduy
        sel = 1
        if (!is.null(input$study_rows_selected))
            sel = input$study_rows_selected
        studies = StudiesDB[sel,"ImmPort.Study.ID"]
        UserState$studies = studies

        # phase 2 all samples
        samples = mgrep(studies, SamplesDB$ImmPort.Study.ID)
        db = SamplesDB[samples, ]
        if (! is.null(UserState$uploadedScored)) {
            user = UserState$uploadedScored
            db = rbind(user, db)
        }
        rownames(db) = db$Biosample.ID


        # phase 3 selected samples
        UserState$DB = db
        
        sel = input$DB_rows_selected
        if (is.null(sel)) {
          sel = RestoreState$samples
        } 
        UserState$SelectedDB = db[sel,]
        UserState$selected = rownames(UserState$SelectedDB )
        
        session$doBookmark()
  })

  onRestored(function(state) {
    RestoreState$selected = strsplit(state$values$samples,",")
    RestoreState$studies = state$values$studies
  })
  
  onBookmarked(function(url) {
        updateQueryString(url)
  })


  onBookmark(function(state) {
    state$values$savedTime <- Sys.time()
    state$values$samples <- paste(UserState$selected, collapse=",")
    state$values$studies <- UserState$studies
  })


  

} # end of server.R singletonfunction

