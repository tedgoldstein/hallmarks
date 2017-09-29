############################################################################################
 


library(urltools)

library(shiny)
library(shinyjqui)

# Don't use (jsonlite) 
library(RJSONIO)

printf <- function(...) cat(sprintf(...))

# To be called from ui.R
radarChartOutput <- function(inputId, width="100%", height="400px") {
  style <- sprintf("width: %s; height: %s;",
    validateCssUnit(width), validateCssUnit(height))
  
  tagList(
    # Include CSS/JS dependencies. Use "singleton" to make sure that even
    # if multiple radarChartOutputs are used in the same page, we'll still
    # only include these chunks once.
    singleton(tags$head(
      tags$script(src="d3/d3.v3.min.js"),
      tags$script(src="radar-chart-binding.js"),
      tags$script(src="OMFScell.js"),
      tags$script(src="handsontable.full.js"),
      tags$link(rel="stylesheet", type="text/css", href="handsontable.full.css"),
      tags$link(rel="stylesheet", type="text/css", href="radar-chart.css"),
      tags$link(rel="stylesheet", type="text/css", href="OMFS.css"),
      tags$script(src="radar-chart.js")
    )),
    div(id=inputId, class="hallmark-chart", style=style, tag("svg", list()))

  )
}

renderRadarChart <- function(expr, env=parent.frame(), quoted=FALSE) {
  # This piece of boilerplate converts the expression `expr` into a
  # function called `func`. It's needed for the RStudio IDE's built-in
  # debugger to work properly on the expression.

  installExprFunction(expr, "func", env, quoted)
  # func <- shiny::exprToFunction(expr, env, quoted)
  
  function() {
    RJSONIO::toJSON(func())
  }
}


renderRadarChart0 <- function(expr, env=parent.frame(), quoted=FALSE) {
  # This piece of boilerplate converts the expression `expr` into a
  # function called `func`. It's needed for the RStudio IDE's built-in
  # debugger to work properly on the expression.

  # installExprFunction(expr, "func", env, quoted)
  func <- shiny::exprToFunction(expr, env, quoted)
  
  function() {
    dataframe <- func()

    mapply(function(col, name) {

      values <- mapply(function(val, i) {
        list(x = i, y = val)
      }, col, 1:nrow(dataframe), SIMPLIFY=FALSE, USE.NAMES=FALSE)

      list(key = name, values = values)
      
    }, dataframe, names(dataframe), SIMPLIFY=FALSE, USE.NAMES=FALSE)
  }
}


simpleCap <- function(x) {
  listOfWords <- strsplit(x, "[-_ .]")
  listOfWords = lapply(listOfWords, function(s) {
      paste0(toupper(substring(s, 1,1)), substring(s, 2))
  })
  paste(unlist(listOfWords), sep="", collapse=" ")
}


# Signatures <- RJSONIO::fromJSON("../Signatures/signatures")
Signatures <- RJSONIO::fromJSON("signatures")
Tissues <- names(Signatures$index)

TCGA = data.frame();


fix <- function(df, r, PI) {
    df[r, "Type"] = simpleCap(sig$cancer)
    df[r, "Subtype"] = simpleCap(sig$tissue)
    df[r, "Species"] = "Homo Sapien"
    df[r, "Study.Title"] <- "Mean average of samples"

    df[r, "PI"] <- PI
    df[r, "ImmPort.Study.ID"] <- "REF"
    df[r, "PubMed"] <- "none"
    df[r, "Experiment.ID"] <- "none"
    df[r, "Cohort"] <- "none"
    df[r, "Biosample.ID"] <- r
    df[r, "Repository.Accession"] <- "none"
    df[r, "Biosample.Name"] <- "none"
    df[r, "Biosample.Description"] <- "none"
    df[r, "Strain"] <- "none"
    return(df)
}

for (sig in Signatures$signatures) {
    cancer = simpleCap(sig$cancer)
    TCGA[cancer, sig$hallmark] = round(mean( sig$reference$score[sig$reference$labels == 1] ))
    TCGA <<- fix(TCGA, cancer, "TCGA")
}

# colnames(TCGA) <- unlist(lapply(colnames(TCGA), function(x) gsub("Tumor.", "Tumor.", gsub(" ", "_", x))))


S = NULL
T = NULL

aggregateScores = function() {
    primary = function(x) {
        df = read.table(paste0("../Scores/",x), sep="\t", header=1, as.is=TRUE)

        if (is.null(S))
            S <<- df
        else {
            S <<- rbind(S, df)
        }
    }
    annotate = function(x) {
        df = read.table(paste0("../Scores/",x), sep="\t", header=1, as.is=TRUE, fill=TRUE)
        df =  merge(S, df, by="Biosample.ID")

        if (is.null(T))
            T <<- df
        else
            T <<- rbind(T, df)
    }

    lapply(list.files(path = "../Scores", pattern = "*.metadata" ), primary)
    lapply(list.files(path = "../Scores", pattern = "*.score" ), annotate)
    df = as.data.frame(T)
    colnames(df) <- gsub("-", "_", colnames(df))
    return(df)
}





read.table.hot = function(name)  {
    table = read.table(name, header=TRUE, as.is=TRUE, fill=TRUE, sep="\t")
    row.names(table) = table$Biosample.ID

    colOrder = colnames(table)
    table = rbind(TCGA, table)
    table = table[, colOrder];
}

# DB <- reactiveFileReader(1000, NULL, 'DB.txt', read.table.hot)
DB = aggregateScores

SamplesDB = aggregateScores()
StudiesDB = SamplesDB[,c("Cancer.Type", "Study.Title", "ImmPort.Study.ID", "PI")]
StudiesDB$Cancer.Type = lapply(StudiesDB$Cancer.Type, simpleCap)
StudiesDB = unique(StudiesDB)
rownames(StudiesDB) = StudiesDB$ImmPort.Study.Id
# rownames(StudiesDB) = do.call(paste, StudiesDB)


Cancers = unique(unlist(lapply(Signatures$signatures,function(s) s$cancer)))

# Mus_Homologues = read.table("Mus_Homologues.txt", header=T, row.names=1, sep="\t")

enableBookmarking(store = "url")

