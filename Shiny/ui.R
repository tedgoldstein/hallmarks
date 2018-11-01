jsCode = '$(".StudyDiv").prependTo(".dt-buttons")'

function(request) {
  shinyjs::useShinyjs()
  SelectStudy <- function()
	div(id = "studyform",
	    style="width:100%",
  	    tags$a(name="Select_Studies", h3("Select Studies")),
	    actionButton("clearStudies", "Clear All Selected"),
            DT::dataTableOutput('study')
        )

  SelectSample <- function()
        div(id = "sampleform",
	    style="width:100%",
            tags$a(name="Select_Samples", h3("Select Samples")),
            downloadButton("downloadSamples", "Download Samples"),
            actionButton("clearSamples", "Clear All Selected"),
            DT::dataTableOutput('DB')
        )

  Visualize <- function() 
     sidebarLayout(
      mainPanel(width=7,
        plotOutput("radarImage", width="800px", height="800px")),

      sidebarPanel(width=5,
        h2("Legend"),
        uiOutput("Legend")
      )
     ) 

  Upload <- function() 
    div(
       wellPanel(
          tags$a(name="Upload_Data", h3("Upload Data")),
          fileInput('file1', 'Choose file to upload',
                    accept = c(
                      'text/tab-separated-values',
                      'text/plain',
		      '.txt',
                      '.csv',
                      '.tsv'
                    )),
          tags$hr(),
          p('Sample file:',
             a(href = 'min.txt', 'min.txt')
          ),
          DT::dataTableOutput('Uploaded')
      ),
      wellPanel(
          p("Select the correct cancer pathology type"),
          selectInput('Cancer', 'Cancer', Cancers) ,
          DT::dataTableOutput('Scored')
      )
   )




# Define UI for random distribution application 
fluidPage(
  tags$head(includeScript("google-analytics.js")),
    
  titlePanel("Oncology Model Fidelity Score"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
    
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
    tabsetPanel(type = "tabs", 
       tabPanel( "Visualize", Visualize()),
       tabPanel( "Select Study", SelectStudy()),
       tabPanel( "Select Sample", SelectSample()),
       tabPanel( "Upload", Upload() ))
    )
}


