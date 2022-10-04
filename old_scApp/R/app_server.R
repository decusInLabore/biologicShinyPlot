#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_scFeatureView_server("scFeatureView_1")
  #mod_biologicHeatmap_server("biologicHeatmap_1")
  #mod_biologicHeatmap_server("biologicHeatmap_2")
  #mod_printTable_server("printTable_1")
  mod_dev_server("dev_1", text = "test test")
  #mod_biologicScatter_server("biologicScatter_1")
  #mod_pcaScatter_server("pcaScatter_1")
}
