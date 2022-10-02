#' biologicHeatmap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dev_ui <- function(id, title){
  ns <- NS(id)
  tabPanel(
    title = title, 
    sidebarPanel(
      shiny::helpText( "Dev Text" )
    ),
    mainPanel(
      shiny::textOutput(ns("text1"))
    )
  )
}

#' biologicHeatmap Server Functions
#'
#' @noRd 
mod_dev_server <- function(id, text = "dev text"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #text <- "text text"
    print(text)
    output$text1 <- renderText(print(text))
    
  })
}

## To be copied in the UI
# mod_printTable_ui("printTable_1")

## To be copied in the server
# mod_printTable_server("printTable_1")
