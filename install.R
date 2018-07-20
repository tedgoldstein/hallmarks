insallIfnecessary <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE , repo="https://cloud.r-project.org/")
      #  Load package after installing
      require( i , character.only = TRUE) 
    }
  }
}

#  Then try/install packages...
insallIfnecessary( c(
    "data.table",
    "dplyr",
    "gelnet",
    "gplots",
    "igraph",
    "Matrix",
    "png",
    "pracma",
    "RJSONIO",
    "shiny",
    "shinyjqui",
    "urltools"
    )
)
if( ! require( "qusage" , character.only = TRUE ) ){
    source("https://bioconductor.org/biocLite.R")
    biocLite("qusage")
}
