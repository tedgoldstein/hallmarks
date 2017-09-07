jsCode = '$(".CancerDiv").prependTo(".dt-buttons")'

function(request) {
     Visualize <- function()
        div(style="width:100%",
            div(class="center",
                    tags$div(class="legend-div",
                        tags$p("Legend"),
                        tags$ul(class="legend-ul")),
                    # checkboxInput("zodiac", "Hallmark Zodiac", value = TRUE, width = NULL),
                    radarChartOutput("radarchart"),


                div(class="CancerDiv",
                    selectInput('cancer', NULL, Studies, width="500px", selectize=TRUE)),

                DT::dataTableOutput('DB')
               ))

    Upload <- function() 
    div(
       wellPanel(
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
          p("Select the correct tissue of origin or cancer pathology determined type"),
          selectInput('tissue', 'Tissue', Tissues) ,
          DT::dataTableOutput('Scored')
      )
   )



    fluidPage(
      title = "Oncology Model Fidelity Score",
      tabPanel('Visualize', id='Visualize', Visualize()),
      tabPanel('Upload',    id='Upload',    Upload())
    )
}

