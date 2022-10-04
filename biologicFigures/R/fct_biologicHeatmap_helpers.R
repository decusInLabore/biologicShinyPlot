#' biologicHeatmap_helpers 
#'
#' @description Plot function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
plotHeatmap <- function(x_axis, y_axis){
      f1 = circlize::colorRamp2(seq(-4, 4, length = 3), c("#3060cf", "#fffbbc","#c4463a")) 
      set.seed(123)
      mat1 = matrix(rnorm(80, 2), 8, 10)
      mat1 = rbind(mat1, matrix(rnorm(40, -2), 4, 10))
      rownames(mat1) = paste0("R", 1:12)
      colnames(mat1) = paste0("C", 1:10)
      
      mat2 = matrix(runif(60, max = 3, min = 1), 6, 10)
      mat2 = rbind(mat2, matrix(runif(60, max = 2, min = 0), 6, 10))
      rownames(mat2) = paste0("R", 1:12)
      colnames(mat2) = paste0("C", 1:10)
      
      le = sample(letters[1:3], 12, replace = TRUE)
      names(le) = paste0("R", 1:12)
      
      ind = sample(12, 12)
      mat1 = mat1[ind, ]
      mat2 = mat2[ind, ]
      le = le[ind]
      
      t1 = ComplexHeatmap::Heatmap(mat1, name = "rnorm")
      ht2 = ComplexHeatmap::Heatmap(mat2, name = "runif")
      ht3 = ComplexHeatmap::Heatmap(le, name = "letters")
      
      p1 <- ht1 + ht2 + ht3
      
      return(p1)
}
