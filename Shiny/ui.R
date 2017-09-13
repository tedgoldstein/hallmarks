jsCode = '$(".StudyDiv").prependTo(".dt-buttons")'

function(request) {
     Visualize <- function()
        div(style="width:100%",
            tags$a(href="#Select_Study", "Select Study"),
            HTML("&nbsp;&nbsp;"),
            tags$a(href="#Select_Sample", "Select Sample"),
            HTML("&nbsp;&nbsp;"),
            tags$a(href="#Upload_Data", "Upload Data"),


            div(class="center",
                    tags$div(class="legend-div",
                        tags$h3("Legend"),
                        tags$ul(class="legend-ul")),
                    # checkboxInput("zodiac", "Hallmark Zodiac", value = TRUE, width = NULL),
                    radarChartOutput("radarchart"),

                tags$a(name="Select_Study", h3("Select Study")),
                div(class="StudyDiv",
                    selectInput('study', NULL, rownames(StudiesDB), width="800px", selectize=TRUE))

               ))
     SelectSample <- function()
        div(style="width:100%",
                tags$a(name="Select_Sample", h3("Select Sample")),
                downloadButton("downloadSamples", "Download Samples"),
                DT::dataTableOutput('DB')
               )

    Upload <- function() 
    div(
       wellPanel(
          tags$a(name="Upload_Data", h3("Upload Data")),
          fileInput('file1', 'Choose file to upload',
                    accept = c(
                      'text/tab-separated-values',
                      'text/plain',
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



    fluidPage(
      title = "Oncology Model Fidelity Score",
      h3("Oncology Model Fidelity Score"),
      Visualize(),
      SelectSample(),
      Upload()
      # tabsetPanel(
      #   tabPanel( "Select Sample", SelectSample()),
      #   tabPanel( "Upload", Upload() ))
    )
}

