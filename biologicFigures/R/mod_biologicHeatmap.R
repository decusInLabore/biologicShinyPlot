#' biologicHeatmap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_biologicHeatmap_ui <- function(id, title){
  ns <- NS(id)
  tabPanel(
    title = title, 
    sidebarPanel(
      shiny::helpText( "Help text" ),
      selectizeInput(
        ns("x_axis"), 
        label = "X axis",
        choices = colnames( mtcars ), 
        selected = colnames( mtcars )[1], 
        options = list(maxOptions = 50)
      ),
      selectizeInput(
        ns("y_axis"), 
        label = "Y axis",
        choices = colnames( mtcars ), 
        selected = colnames( mtcars )[2], 
        options = list(maxOptions = 50)
      )
    ),
    mainPanel(
      plotOutput(ns("plot1"))
    )
  )
}
    
#' biologicHeatmap Server Functions
#'
#' @noRd 
mod_biologicHeatmap_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$plot1 <- renderPlot({
      p1 <- plotHeatmap(x_axis = input$x_axis, y_axis = input$y_axis)
      print(p1)
    })
 
  })
}
    
## To be copied in the UI
# mod_biologicHeatmap_ui("biologicHeatmap_1")
    
## To be copied in the server
# mod_biologicHeatmap_server("biologicHeatmap_1")
