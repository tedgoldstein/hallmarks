############################################################################################
df = read.csv("DB.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)
y <- aggregate(Study.Title ~ Type, df, c)
rownames(y) <- y$Type
TST = apply(y, 1, function(x) as.list(unique(unlist(x$Study.Title))))
 


library(shiny)
library(shinyjqui)

# Don't use (jsonlite) 
library(RJSONIO)

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
    df[r, "BioSample.ID"] <- c
    df[r, "Repository.Accession"] <- "none"
    df[r, "Biosample.Name"] <- "none"
    df[r, "Biosample.Description"] <- "none"
    df[r, "Strain"] <- "none"
    return(df)
}

for (sig in Signatures$signatures) {
    m = round(mean( sig$reference$score[sig$reference$labels == 1] ))
    c = simpleCap(sig$cancer)
    TCGA[c, sig$hallmark] = m
    TCGA = fix(TCGA, c, "TCGA")
}

# colnames(TCGA) <- unlist(lapply(colnames(TCGA), function(x) gsub("Tumor.", "Tumor.", gsub(" ", "_", x))))



read.table.hot = function(name)  {
    table = read.table(name, header=TRUE, as.is=TRUE, fill=TRUE, sep="\t")
    row.names(table) = table$BioSample.ID

    colOrder = colnames(table)
    table = rbind(TCGA, table)
    table = table[, colOrder];
}

DB <- reactiveFileReader(1000, NULL, 'DB.txt', read.table.hot)

SamplesDB = isolate(DB())



# Cancers = c("All", unique(sort(SamplesDB$Type)))
Cancers = unique(sort(SamplesDB$Type))

SelectStudies = function(db) {
   fields = c("ImmPort.Study.ID", "PI",  "Study.Title")
   c("All", unique(sort(apply(unique(db[,fields]), 1, function(x) paste(x, collapse=" ")))))
}


Studies = SelectStudies(SamplesDB)

Mus_Homologues = read.table("Mus_Homologues.txt", header=T, row.names=1, sep="\t")

enableBookmarking(store = "url")

