#' pcaScatter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 


    
xOptions <- get_options(tag = "PC", table = "pcaTB")

yOptions <- get_options(tag = "PC", table = "pcaTB")




mod_pcaScatter_ui <- function(id, title){
  ns <- NS(id)
  tabPanel(
    title = title, 
    sidebarPanel(
      shiny::helpText( "Help text" ),
      selectizeInput(
        ns("x_axis"), 
        label = "X axis",
        choices = xOptions,  
        selected = xOptions[1]
      ),
      selectizeInput(
        ns("y_axis"), 
        label = "Y axis",
        choices = yOptions, 
        selected = yOptions[ 2 ]
      ),
      selectizeInput(
        ns("colorBy"), 
        label = "colorBy",
        choices = c("sample_group", "sample_id"), 
        selected = "sample_group"
      ),
      uiOutput(ns('pcaColorPanel'))
    ),
    mainPanel(
      plotOutput(ns("pcaScatter1"))
    )
  )
}
    
#' pcaScatter Server Functions
#'
#' @noRd 
mod_pcaScatter_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    dfkey <- assembleKeyList()$dfkey
    
    #dfData <- get_data()
    
    output$pcaColorPanel <- renderUI({ 
      
      data <- get_data(
        table = "pcaTB",
        colSel = c("sample_id", "sample_group", input$x_axis, input$y_axis)
      )
      
      lev <- sort(unique(as.vector(data[,input$colorBy]))) # sorting so that "things" are unambigious
      print(lev)
      cols <- scales::hue_pal()(length(  lev ) )
      
      # New IDs "colX1" so that it partly coincide with input$select...
      lapply(seq_along(lev), function(i) {
        colourpicker::colourInput(
            inputId = ns(paste0("col", lev[i])),
            label = paste0("Choose colour for ", lev[i]), 
            value = cols[i]
        )        
      })
    })
    
    output$pcaScatter1 <- renderPlot({
      
      data <- get_data(
        table = "pcaTB",
        colSel = c("sample_id", "sample_group", input$x_axis, input$y_axis)
      )
      
      cols <- paste0("c(", paste0("input$col", as.vector(data[,input$colorBy]), collapse = ", "), ")")
      cols <- eval(parse(text = cols))
     
      
      # To prevent errors
      req(length(cols) == length(as.vector(data[,input$colorBy])))
      print(cols)
      
      #cols <- eval(parse(text = cols))
      
      #req(length(cols) == length(unique(data[,input$colorBy])))
      
      #p1 <- plotPcaScatter(data = data, x_axis = input$x_axis, y_axis = input$y_axis, colorBy = input$colorBy, colors = cols)
      #print(p1)
      plot(1:10,1:10)
    })
 
  })
}
    
## To be copied in the UI
# mod_pcaScatter_ui("pcaScatter_1")
    
## To be copied in the server
# mod_pcaScatter_server("pcaScatter_1")
