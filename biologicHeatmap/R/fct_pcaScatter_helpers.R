#' biologicHeatmap_helpers 
#'
#' @description Plot function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 


get_data <- function(
  table = "mainTB",
  colSel = " * "
){
  keyList <- assembleKeyList()
  if (keyList[["dataMode"]] == "SQLite"){
    
    dbDB <- DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname=keyList[["dbname"]]
    )
    
  } else {
    
    dbDB <- DBI::dbConnect(
      drv = RMySQL::MySQL(),
      user = keyList[["user"]], 
      password = keyList[["DBpd"]], 
      host = keyList[["host"]], 
      dbname=keyList[["dbname"]]
      
    )
    
  }
  
  #dbDB <- RMySQL::dbConnect(RMySQL::MySQL(), user = user, password = DBpd, host = host, dbname=dbname)
  query <- paste0("SELECT DISTINCT ", paste0(colSel, collapse = ", ")," FROM ", keyList$dfkey[[table]])
  dfTable <- DBI::dbGetQuery(dbDB, query)
  
  RMySQL::dbDisconnect(dbDB)
  
  return(dfTable)
}

get_options <- function(
  tag = c("contrast"),
  table = "mainTB"
){
  keyList <- assembleKeyList()
  if (keyList[["dataMode"]] == "SQLite"){
    
    dbDB <- DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname=keyList[["dbname"]]
    )
    
  } else {
    
    dbDB <- DBI::dbConnect(
      drv = RMySQL::MySQL(),
      user = keyList[["user"]], 
      password = keyList[["DBpd"]], 
      host = keyList[["host"]], 
      dbname=keyList[["dbname"]]
      
    )
    
  }
  
  #dbDB <- RMySQL::dbConnect(RMySQL::MySQL(), user = user, password = DBpd, host = host, dbname=dbname)
  query <- paste0("SELECT DISTINCT * FROM ", keyList$dfkey[[table]], " LIMIT 0,1;")
  dfTable <- DBI::dbGetQuery(dbDB, query)
  
  RMySQL::dbDisconnect(dbDB)
  
  allNames <- names(dfTable)
  
  allOptions <- as.vector(rbind(unlist(purrr::map( 1:length( tag ), function( i ) {
    res <- allNames[grep(tag[ i ], allNames)]
  }))))
  
  return(allOptions)
}

plotPcaScatter <- function(data = mtcars, x_axis = "mpg", y_axis="cyl", colorBy="sample_group", colors){
    print("Colors")
    print(colors)
    p1 <- ggplot2::ggplot(
        data = data, ggplot2::aes_string(x = x_axis, y = y_axis, fill = colorBy)) +
        ggplot2::geom_point(shape =21) +
        ggplot2::scale_fill_manual(values = colors)
    return(p1)
      
}
