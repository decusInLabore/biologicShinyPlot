#' biologicHeatmap_helpers 
#'
#' @description Plot function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
plotTable <- function(columnSelection){
    DT::datatable(mtcars[,columnSelection], filter="top",options = list(lengthChange = FALSE) )
      
}
