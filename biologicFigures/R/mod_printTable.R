#' biologicHeatmap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_printTable_ui <- function(id, title){
  ns <- NS(id)
  tabPanel(
    title = title, 
    sidebarPanel(
      shiny::helpText( "Table" ),
      shiny::checkboxGroupInput(
          ns("columnSelection"), 
          "Variables to show:",
              c(
                  "Cylinders" = "cyl",
                  "Transmission" = "am",
                  "Gears" = "gear",
                  "Mpg" = "mpg"
                ),
          selected= c("cyl", "am", "gear")
      )
    
    ),
    mainPanel(
      DT::DTOutput(ns("table1"))
    )
  )
}

#' biologicHeatmap Server Functions
#'
#' @noRd 
mod_printTable_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$table1 <- DT::renderDT({
        plotTable(columnSelection = input$columnSelection)
    })
    
    
    
  })
}

## To be copied in the UI
# mod_printTable_ui("printTable_1")

## To be copied in the server
# mod_printTable_server("printTable_1")
