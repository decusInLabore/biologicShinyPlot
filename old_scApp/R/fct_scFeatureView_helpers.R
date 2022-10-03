#' biologicHeatmap_helpers 
#'
#' @description Plot function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
###############################################################################
## Load dfCoord from db                                                      ##

createDfCoord <- function(
    startUpList
){
    oldw <- getOption("warn")
    options(warn = -1)
    #startUpList <- golem::get_golem_options(which = "startUpList")
    keyList <- startUpList$keyList
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
    query <- paste0("SELECT DISTINCT * FROM ", keyList$coordinateTbName)
    dfCoordSel <- DBI::dbGetQuery(dbDB, query)
    
    RMySQL::dbDisconnect(dbDB)
    
    dfCoordSel$row_names <- NULL
    dfCoordSel[["all"]] <- "all"  
    
    
    
    dfCoordSel$cellID <- gsub("[.]", "-", dfCoordSel$cellID)
    dfCoordSel$cellID <- gsub("-", "_", dfCoordSel$cellID)
    
    return(dfCoordSel) 
    
}
#end_time <- Sys.time()
#print(paste0("Q S1 DBQ Coordinates: ",end_time - start_time))
##                                                                           ##
###############################################################################

###############################################################################
## Create Color Table                                                        ##

createColorTable <- function(
    startUpList,
    colorBy = "lg10Expr"
){
    dfDL <- createDfCoord(startUpList = startUpList)
    #startUpList <- golem::get_golem_options(which = "startUpList")
    dfColOptions <- startUpList$dfColOptions
    #######################################################################
    ## Check if colors are available                                     ##
    colorAnnoFound <- FALSE
    
    if (!is.null(dfColOptions)){
        dfPlotCols <- dfColOptions[dfColOptions$menuName == colorBy, c("colOption", "colSel")]
        
        if (nrow(dfPlotCols) > 0){
            checkDLvec <- sort(na.omit(as.vector(unique(dfDL[,colorBy]))))
            checkColvec <- sort(na.omit(unique(dfPlotCols$colOption)))
            if (identical(checkDLvec, checkColvec)){
                dfAddCol <- unique(dfPlotCols)
                names(dfAddCol) <- gsub("colOption", colorBy, names(dfAddCol))
                names(dfAddCol) <- gsub("colSel", "dotColor", names(dfAddCol))
                
                dfDL[,colorBy] <- as.character(dfDL[,colorBy])
                dfAddCol[,colorBy] <- as.character(dfAddCol[,colorBy])
                
                
                dfDL <- dplyr::full_join(
                    dfDL,
                    dfAddCol,
                    by=colorBy
                )
                
                
                dfDL[is.na(dfDL)] <- ""
                selVec <- c(colorBy, "dotColor")
                dfDL <- unique(dfDL[,selVec])
                colorAnnoFound <- TRUE
            }
            
        }
        
    } 
    
    
    ## Done                                                              ##
    #######################################################################
    
    
    
    dfDL[["lg10Expr"]] <- "A1"
    
    
    if(!colorAnnoFound) {
        dfDL[["dotColor"]] <- "#000000"
        selVec <- c(colorBy, "dotColor")
        dfDL <- unique(dfDL[,selVec])
        dfDL <- dfDL[order(dfDL[,1], decreasing = F),]
        dfDL[,colorBy] <- factor(dfDL[,colorBy])
        
        dfDL[["dotColor"]] <- scales::hue_pal()(nrow(dfDL))
    }
    
    #}
    
    dfDL <- dfDL[dfDL[,colorBy] != "", ]
    dfDL <- dfDL[!is.na(dfDL[,colorBy]), ]
    
    return(dfDL)
}

##                                                                       ##
###########################################################################

#########################################################################
## Retrieve Coordinates for this query

## Done retrieving Coordinates
#########################################################################

###########################################################################
## Database query for dfExpr                                             ##
## create agl315_gene_expr_tb
#start_time <- Sys.time()
createDfExprSel <- function(
    startUpList,
    gene
    
){
    oldw <- getOption("warn")
    options(warn = -1)
    # startUpList <- golem::get_golem_options(which = "startUpList")
    keyList <- startUpList$keyList
    
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
    
    if ( is.null(gene) | gene == "" ){
        query <- paste0("SELECT * FROM ",keyList$exprTbName," WHERE gene = '",keyList$geneDefault,"'" )  
    } else {
        query <- paste0("SELECT * FROM ",keyList$exprTbName," WHERE gene = '",gene,"'" )
    }
    
    #query <- paste0("SELECT DISTINCT * FROM agl315_gene_expr_tb WHERE gene = 'GFAP'" )
    dfExprSel <- DBI::dbGetQuery(dbDB, query)
    DBI::dbDisconnect(dbDB)
    
    names(dfExprSel) <- gsub("condition", "cellID", names(dfExprSel))
    names(dfExprSel) <- gsub("^expr$", "lg10Expr", names(dfExprSel))
    dfExprSel$cellID <- gsub("[.]", "-", dfExprSel$cellID)
    dfExprSel$cellID <- gsub("-", "_", dfExprSel$cellID)
    dfExprSel$cellID <- gsub("^X", "", dfExprSel$cellID)
    return(dfExprSel)
}

#end_time <- Sys.time()
#print(paste0("Q S2 agl315_gene_expr_tb: ",end_time - start_time))
#paste0("SELECT DISTINCT gene, condition, expr FROM agl315_gene_expr_tb WHERE gene = '",gene,"'" )
## Done db query                                                         ##
###########################################################################

###############################################################################
## Create dfTemp                                                             ##       
createDfTemp <- function(
    startUpList,
    gene,
    splitByColumn,
    x_axis, 
    y_axis
){
    
    dfTemp <- dplyr::full_join(
        createDfCoord(startUpList = startUpList), 
        createDfExprSel(startUpList = startUpList, gene = gene), 
        by="cellID"
    )
    
    
    #dfTemp2 <- merge(createDfCoord(), createDfExprSel(), by.x = "cellID", by.y="cellID", all=TRUE)
    dfTemp[is.na(dfTemp)] <- 0
    dfTemp <- data.frame(dfTemp, stringsAsFactors = FALSE)
    dfTemp$gene <- as.character(dfTemp$gene)
    
    conditionVec <- sort(unique(dfTemp[,splitByColumn]))  
    
    #######################################################################
    ## Check if custom colors are to be used                             ##
    
    dfTemp[["Dcolor"]] <- dfTemp[,colorBy]
    
    dfTemp[["dotColor"]] <- "#000000"
    
    #if (colorBy == "clusterName"){
    #    dfTemp[["dotColor"]] <- dfTemp[["clusterColor"]]
    #} else if (colorBy == "sampleName"){
    #    dfTemp[["dotColor"]] <- dfTemp[["sampleColor"]]
    #}
    
    inInput <- names(input)[names(input) %in% unique(dfTemp[[colorBy]])]
    if (length(inInput) > 0){
        for (k in 1:length(inInput)){
            dfTemp[dfTemp[,colorBy] == inInput[k], "dotColor"] <- input[[inInput[[k]]]]
        }
        
    }
    
    
    ## Done                                                              ##
    #######################################################################
    
    pos <- grep(paste0("^", x_axis, "$"), names(dfTemp))
    if (length(pos) > 0){
        dfTemp[["x_axis"]] <- dfTemp[,x_axis]
    } else {
        dfTemp[["x_axis"]] <- x_axis
    }
    
    
    if (!is.numeric(dfTemp$x_axis)){
        dfTemp$x_axis <- factor(dfTemp$x_axis, levels = sort(unique(dfTemp$x_axis)))
    }
    
    
    ## We need to consider cases like Densityplot and Histogram, where y_axis is not a column
    if (y_axis %in% names(dfTemp)){
        dfTemp[["y_axis"]] <- dfTemp[,y_axis]
    } else {
        dfTemp[["y_axis"]] <- y_axis
    }
    
    
    #dfTemp <- dfTemp[,selVec]  
    dfTemp <- dfTemp[(dfTemp$x_axis != 0 | dfTemp$y_axis != 0),] 
    #dfTemp
    
    #################
    ## Create plot select
    #plot_select <- reactive({
    df <- dfTemp
    df[["all"]] <- "all"
    plot_select <-  as.vector(unique(df[, splitByColumn]))
    #})
    ## Done Creating plot select
    ####################
    
    ####################
    ## Create plot data names
    plot_data_names <- sort(as.vector(unique(dfTemp[, splitByColumn])))
    ##
    ####################
    
    ###################
    ## get max expr
    #maxExpr <- reactive({
    #  dfTemp <- createDfTemp()
    
    if (is.numeric(dfTemp$Dcolor)){
        maxExpr <- max(as.vector(dfTemp$Dcolor))
    } else{
        maxExpr <- NULL
    }
    #    return(maxExpr)
    #  })
    
    ##
    ####################
    
    ###################
    ## plot data
    #plot_data <- reactive({
    #  dfTemp <- createDfTemp()
    
    #plot_select <- plot_data_names()
    
    plot_data <- lapply(plot_data_names, function(x) dfTemp[dfTemp[,splitByColumn] == x,])
    #})
    ##
    ###################
    
    ######################
    ## min/max
    #determinePlotDims <- reactive({
    #  dfTemp <- createDfTemp()
    
    if (!is.numeric(dfTemp$x_axis)){
        minX <- 0
        maxX <- length(unique(dfTemp$x_axis)) + 1
    } else {
        maxX <- 1.1*max(dfTemp$x_axis, na.rm = T)
        minX <- 1.1*min(dfTemp$x_axis, na.rm = T)
    }
    
    if (!is.numeric(dfTemp$y_axis)){
        minY <- 0
        maxY <- length(unique(dfTemp$y_axis)) + 1
    } else {
        minY <- 1.1*min(dfTemp$y_axis, na.rm = T)
        maxY <- 1.1*max(dfTemp$y_axis, na.rm = T)
    }
    
    
    dimVec <- c(minX, maxX, minY, maxY)
    dimVec
    #})
    
    ##
    #######################
    
    returnList <- list(
        "dfTemp" = dfTemp,
        "plot_select" = plot_select,
        "plot_data_names" = plot_data_names,
        "plot_data" = plot_data,
        "maxExpr" = maxExpr,
        "dimVec" = dimVec
    )
    
    return(returnList)
    
}
##                                                                           ##
###############################################################################


