#' biologicHeatmap_helpers 
#'
#' @description Plot function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
plotScatter <- function(data = mtcars, x_axis = "mpg", y_axis="cyl"){
    p1 <- ggplot2::ggplot(
        data = data, ggplot2::aes_string(x = x_axis, y = y_axis)) +
        ggplot2::geom_point()
    return(p1)
      
}
