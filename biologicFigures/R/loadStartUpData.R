###############################################################################
## Load data required on startup                                             ##


###############################################################################
## Load key file                                                             ##
assembleKeyList <- function(){
    
    
###############################################################################
## Data Access Module                                                        ##
    
    FNkey <- "data/dbConnect/db.txt"
    
    if (file.exists(FNkey)){
        ## Option to be used if installed as local app ##
        dfkey <- read.delim(FNkey, stringsAsFactors = F, sep="\t")
        loadFile <- NULL
    } else if (file.exists("data/plotData/plotData.txt")){
        loadFile <- "data/plotData/plotData.txt"
        dfkey <- NULL
    } else {
        print("No data input")
    }
    
    if (!is.null(dfkey)){
        keyList <- list()
        keyList[["geneDefault"]] = as.character(dfkey$default)
        keyList[["host"]] <- as.character(dfkey$url)
        keyList[["user"]] <- as.character(dfkey$id)
        keyList[["DBpd"]] <- as.character(dfkey$id2)
        keyList[["dbname"]] <- as.character(dfkey$db)
        keyList[["coordinateTbName"]] <- as.character(dfkey$coordTb)
        keyList[["exprTbName"]] <- as.character(dfkey$exprTb)
        keyList[["geneID_TbName"]] <- as.character(dfkey$geneTb)
        
        pos <- grep("experiment", names(dfkey))
        if (length(pos) == 0){
            dfkey[["experiment"]] <- paste0("Experiment_", 1:nrow(dfkey))
        }
        
        ## Decide on database mode ##
        pos <- grep("dataMode", names(dfkey))
        if (length(pos) == 1){
            if (dfkey$dataMode == "SQLite"){
                keyList[["dataMode"]] <- dfkey$dataMode
            } else {
                keyList[["dataMode"]] <- "MySQL"
            }
        } else {
            keyList[["dataMode"]] <- "MySQL"
        }
        
        keyList[["dfkey"]] <- dfkey
    } else {
        keyList[["dataFN"]] <- loadFile
    }
    return(keyList)
}

## Done keyList                                                              ##
###############################################################################




###############################################################################
## Collection function                                                       ##

loadStartUpData <- function(){
    
    startUpList <- list()
    
    startUpList[["keyList"]] <- assembleKeyList()
    
    return(startUpList)
}

## Done                                                                      ##
###############################################################################

