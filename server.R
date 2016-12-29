library(shiny); library(shinythemes)
library(tidyr); library(dplyr)
library(DT); library(lingStuff)

features <- distFeat[[1]]
distFeatDefs <- distFeat[[2]]

features[sapply(features, is.character)] <- lapply(features[sapply(features, is.character)], as.factor)

function(input, output) {

  # Select specification of features for subsetting
  output$ui <- renderUI({

    # for each input selected, get levels and display them 
    # as radio buttons 
    lapply(input$in0, function(var) {
      list(radioButtons(paste0("dynamic_", var), 
                        label = var, 
                        choices = levels(features[[var]])))
    })
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
    print(c(paste0("<font face='Times New Roman' size='3'>",
                   '[ ', 
                   temp[, 3], 
                   ' ]: ', 
                   temp[, 4], 
                   ', ', 
                   temp[, 5], 
                   ', ', 
                   temp[, 6], 
                   "</br>", 
                   collapse = '')))

  })




  # Print definition of selected features
  output$defs = renderDataTable({

    # Create datatable of definitions using selected features
    datatable(distFeatDefs[distFeatDefs$feature %in% input$in0, 3:4], 
              rownames = FALSE, options = list(searching = FALSE, paging = FALSE, 
              scrollY = '300px'))

  })


  # Download data 
  output$downloadData <- downloadHandler(
    filename = 'distFeat.csv', 
    content = function(file) {
      write.csv(features, file)
    }
  )
}


