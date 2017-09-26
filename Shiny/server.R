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
            Strain = rep( "none", n)));

    return (scores)
}




spaceFix <- function (x) gsub("[._]", " ", x)

function(input, output, session) {
  UserState <- reactiveValues();
  
   setBookmarkExclude(c(
        "file1", "Uploaded", 
        # want this "study",
        "DB_cell_clicked",
        "DB_rows_all",
        # want this "DB_rows_current",
        # want this "DB_rows_selected",
        # want this "DB_search",
        "DB_state"
        ))

  study_selected = reactive({
    sel = 1
    if (!is.null(input$study_rows_selected))
        sel = input$study_rows_selected
    StudiesDB[sel,]
  })
   
  radar_colors= rainbow(8)
  rgba = function(x) { 
      y = col2rgb(x)
      paste("rgba(", paste(unname(y), collapse=","), ",0.5)", sep="")
  }
  

  DB = reactive({
    

    study =  study_selected()
    title = study$Study.Title
    UserState$Study.ID  = study$ImmPort.Study.ID # fix

    db = SamplesDB[SamplesDB$Study.Title == title, ]
    TCGA = db[SamplesDB$PI == "TCGA", ]

    # TCGA reference samples should be at the beginning
    type = db[1,"Type"]
    ref = TCGA[TCGA$Subtype == type,]
    db = rbind(ref, db)

    if (! is.null(UserState$uploadedScored)) {
        user = UserState$uploadedScored
        printf("uploadedScored rebound\n");


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
    a = UserState$uploadedScored
    db = DB()[,displayed_columns]
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
    
    if (length(UserState$selected) == 0) {
        selected = c(1, 2)
    } else {
        selected = unlist(lapply(unlist(UserState$selected), function(pat) grep(pat, db$Biosample.ID)))
    }
    DT::datatable(data.frame( ff ), colnames=c, selection = list(selected = selected), escape=FALSE  )
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
    s = input$DB_rows_selected
    if (is.null(s))
       s = c(1,2)

    db = DB()
    ldb = db[s, legend_columns]

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
    
     tags$li( div( tags$span(style=style), tags$span(legend[i])))
  }
    
    tags$ul(style="list-style: none;", lapply(1:length(legend), wrapDiv))
  })



  plotRadarChart = function(zodiacLayout) {
    s = input$DB_rows_selected
    if (is.null(s))
       s = c(1,2)

    db = DB()
    hdb = db[s, hallmark_columns]
    ldb = db[s, legend_columns]
    if (nrow(ldb) > 0)
        legend =  apply(ldb, 1, function(x) paste(x, collapse=" "))
    else
        legend = "none selected"

    rownames(hdb) = ldb$Biosample.ID

    # The main call
    data = hdb

    # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
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
    if (!zodiacLayout)
        legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
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
        DT::datatable(StudiesDB, selection = "single", rownames= FALSE)
    })

    output$Scored <- DT::renderDataTable( { 
        if (! is.null(UserState$uploaded)) {
            UserState$uploadedScored = computeSignatureScore(UserState$uploaded, input$Cancer)
            DT::datatable(UserState$uploadedScored)
            printf("computeSignatureScore %s\n",  input$Cancer)
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
        session$doBookmark()
  })
  onBookmarked(function(url) {
        updateQueryString(url)
  })

  onBookmark(function(state) {
    state$values$savedTime <- Sys.time()
  })


  onRestored(function(state) {
    if (length(state$values$selected) > 0) {
        db = DB()
        selected = strsplit(state$values$selected, ",");
        ix = unlist(lapply(unlist(selected), function(pat) grep(pat, db$Biosample.ID)))
        df = db[ix,]
        if (nrow(df) > 0) {
          updateTextInput(session, "study", value = df[1]$Study.Title)
          UserState$selected = selected
        }
        
    }
  })
  
  

} # end of server.R singletonfunction

