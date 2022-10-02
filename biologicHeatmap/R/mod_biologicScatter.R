#' biologicScatter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_biologicScatter_ui <- function(id, title){
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
      plotOutput(ns("scatter1"))
    )
  )
}
    
#' biologicScatter Server Functions
#'
#' @noRd 
mod_biologicScatter_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$scatter1 <- renderPlot({
      p1 <- plotScatter(data = mtcars, x_axis = input$x_axis, y_axis = input$y_axis)
      print(p1)
    })
 
  })
}
    
## To be copied in the UI
# mod_biologicScatter_ui("biologicScatter_1")
    
## To be copied in the server
# mod_biologicScatter_server("biologicScatter_1")
