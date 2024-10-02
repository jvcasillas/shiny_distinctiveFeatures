library("shiny")
library("shinythemes")
library("tidyr")
library("dplyr")
library("DT")

# Load datasets
load("distFeat.RData")

# Extract dataframes
features <- distFeat[[1]]
distFeatDefs <- distFeat[[2]]

# Set characters as factors
features[sapply(features, is.character)] <- lapply(features[sapply(features, is.character)], as.factor)

for (i in 7:length(features)){
    features[, i] <- factor(features[, i], levels = c("+", "-"))
}


ui <- fluidPage(theme = shinytheme("united"),
  fluidRow(
    column(3,
      titlePanel("Spanish allophones", windowTitle = "Spanish allophones"),
      wellPanel(
        # Drop down of all features
        selectInput(
          inputId = 'in0', 
          label = 'Features', 
          choices = colnames(features[7:length(features)]), 
          multiple = TRUE, selectize = TRUE
        ), 
      
        # Radio buttons for features
        uiOutput("ui"),
      
        # Details
        p(strong("Created by:"), 
        tags$a("Joseph V. Casillas", href="http://www.jvcasillas.com"),
        br(), strong("Source code:"), 
        tags$a("Github", href="https://github.com/jvcasill/shiny_distinctiveFeatures/"))

        # Include download bottom
        #downloadButton('downloadData', 'Download')
      )
    ), 
    column(3, 
      h2(''),
      br(), 
      h4('Segments'), 
      br(),
      htmlOutput("symbols")
    ),
    column(5,
      h2(''),
      br(),
      h4('Feature descriptions'), 
      dataTableOutput('defs'),
      br()
    )
  )
)


server <- function(input, output) {

  # Select specification of features for subsetting
  output$ui <- renderUI({

    # for each input selected, get levels and display them 
    # as radio buttons 
    lapply(input$in0, function(var) {
        list(
          radioButtons(
            paste0("dynamic_", var), 
            label = var, 
            choices = levels(features[[var]])
          )
        )
      }
    )
  })




  # HTML Symbols
  output$symbols = renderText({ 

    # If nothing is selected then return 1st column
    if(is.null(input$in0)) return(print('Select some features!'))
    
    # Create subset from radio output, save as sub
    sub <- function(data, var) {
      idx <- data[[var]] == input[[paste0("dynamic_", var)]]
      data[idx, ]
    }

    # Use sub to subset features dataframe
    temp <- Reduce(f = sub, init = features, x = input$in0)

    # datatable(temp[3:6], rownames = FALSE, options = list(searching = FALSE, paging = FALSE, 
    # scrollY = '300px'))

    # Return allophone column of subset
    print(
      c(
        paste0(
          "<font face='Times New Roman' size='3'>",
          '[ ', 
          temp[, 3], 
          ' ]: ', 
          temp[, 4], 
          ', ', 
          temp[, 5], 
          ', ', 
          temp[, 6], 
          "</br>", 
          collapse = ''
        )
      )
    )
  })

  # Print definition of selected features
  output$defs = renderDataTable({

  # Create datatable of definitions using selected features
  datatable(
    distFeatDefs[distFeatDefs$feature %in% input$in0, 3:4], 
    rownames = FALSE, 
    options = list(searching = FALSE, paging = FALSE, scrollY = '300px')
   )
  })

  # Download data 
  output$downloadData <- downloadHandler(
    filename = 'distFeat.csv', 
    content = function(file) {
      write.csv(features, file)
    }
  )
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

