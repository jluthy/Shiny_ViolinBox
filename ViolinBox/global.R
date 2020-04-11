#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readr)
library(dplyr)
library(tidyr)
library(pheatmap)
library(plotly)
library(viridisLite)
library(DT)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(beeswarm)
library(ggbeeswarm)
library(FlowSOM)

# Choose from the two available csv files in extData folder
# inCSVPath <- file.choose()
inCSVPath <- "/Users/10273835/Documents/code/exTidyData/B1.smallPop.catPar.ExtNode.csv"
# fileName <- system.file("extdata", "lymphocytes.fcs", package="FlowSOM")
df <- read.csv(inCSVPath, sep = ",", header = TRUE, check.names = FALSE)
# ff <- flowCore::read.FCS(fileName)
# df <- ff@exprs[]
catParRaw <- "FlowMeans.ClusterID"
noKatPar <- TRUE
if(catParRaw != "[]" && catParRaw != ""){
  catParam <- gsub("(\\[)","",gsub("(\\])","",catParRaw))
  catPar <- as.symbol(catParam)
  noKatPar <- FALSE
}
if ("EventNumberDP" %in% colnames(df)){
  print("EventNumber Detected - This is FlowJo")
  df <- df %>%
    select(EventNumberDP, everything())
  colnames(df) <- gsub( "(^.*)( :: )" , "", colnames(df), ignore.case = TRUE )
  isSeqGeq <- F
  df$EventNumberDP <- NULL
} else {
  print("No EventNumber Detected - We're in SeqGeq")
  isSeqGeq <- T
}
# Parameter names for plotting
paramNames <- colnames(df)
# parList <- as.list(paramNames)
hmPalette <- c("magma", "plasma", "inferno", "viridis", "cividis", "rainbow")
vbPalette <- c("Dark2","Accent","Pastel2","Set2", "Pastel1", "Set1","Paired","Set3","Spectral","BrBG","PiYG","PRGn","PuOr")
somPalette <- c("Dark2","Set2", "Pastel1", "Set1","Set3","Spectral","BrBG","PiYG","PRGn")

# Quantial Color scale
quantile_breaks <- function(xs, n = 10) {
  breaks <- quantile(xs, probs = seq(0, 1, length.out = n))
  breaks[!duplicated(breaks)]
}
df2 <- as.matrix(df)
mat_breaks <- quantile_breaks(df2, n = 11)

parList <- as.list(paramNames)
if (noKatPar){
  selectACat <- NULL
} else {
  selectACat <- catPar
  for(n in 1:length(parList)){
    if(parList[n] == catParam){
      parList[n] <- NULL
    }
  }
}
###############################
##      CREATE FUNCTIONS     ##
###############################
###########################
## FlowSOM FCS from CSV: ##
###########################
csvPath <- inCSVPath


CellIdDP <- "CellId"
generateGUID <- function(data) {
  mySeed <- round(1000*mean(as.matrix(data))) + dim(data)[1] + dim(data)[2]
  while (mySeed > .Machine$integer.max) mySeed <- round(mySeed/3)
  set.seed(as.integer(mySeed))
  
  part1 <- paste(unlist(lapply(1:8, function(x) { sprintf("%x", round(runif(1, 0, 9))) })), collapse="", sep="")
  part2 <- paste(unlist(lapply(1:4, function(x) { sprintf("%x", round(runif(1, 0, 15))) })), collapse="", sep="")
  part3 <- paste(unlist(lapply(1:4, function(x) { sprintf("%x", round(runif(1, 0, 15))) })), collapse="", sep="")
  part4 <- paste(unlist(lapply(1:4, function(x) { sprintf("%x", round(runif(1, 0, 15))) })), collapse="", sep="")
  part5 <- paste(unlist(lapply(1:12, function(x) { sprintf("%x", round(runif(1, 0, 15))) })), collapse="", sep="")
  paste(part1, part2, part3, part4, part5, sep="-")
}
# Build a FCS file from exported csv from FJ
flowFrameFromFlowJoCSV <- function(csvPath, dropCompPrefix=FALSE) {
  data <- read.csv(csvPath, check.names=FALSE)
  cellIdsColumn <- rep(NA, nrow(data))
  cellIds <- data.frame(CellIdDP = cellIdsColumn)
  names(cellIds) <- CellIdDP
  if (CellIdDP %in% names(data)) {
    cellIds <- data[, CellIdDP]
    data <- data[, -which(names(data) %in% c(CellIdDP))]
  }
  
  colNames <- colnames(data)
  npar <- length(colNames)
  id <- paste("$P",1:npar,sep="")
  
  maxRange <- unlist(lapply(1:npar, function(x) {
    max(data[,x])
  }))
  range <- maxRange + 1
  minRange <- unlist(lapply(1:npar, function(x) {
    min(0, min(data[,x]))
  }))
  
  colNames <- gsub("^\\s+|\\s+$", "", colNames) 	# Trim the leading and tailing whitespaces
  
  ## FlowJo splits parameter names and description by " :: ", e.g., "Comp-PE-Cy5-A :: CD19#PE-Cy5"
  names <- unlist(lapply(1:npar, function(i) {
    unlist(strsplit(colNames[i], " :: "))[1]
  }))
  ## FlowJo indicates that a patameter is compensated by the Comp- prefix in its name, we can drop that if undesirable
  if (dropCompPrefix) {
    names <- gsub("^\\Comp-", "", names)
  }
  ## Again splitting by " :: " but this time taking the second part (if present) as description
  desc <- unlist(lapply(1:npar, function(i) {
    s <- unlist(strsplit(colNames[i], " :: "))
    if (length(s) >= 2) s[2]
    else NA
  }))
  
  pars <- new("AnnotatedDataFrame",
              data=data.frame(
                row.names=I(id), name=I(names), desc=I(desc),
                range=range, minRange=minRange, maxRange=maxRange),
              varMetadata=data.frame(
                row.names=I(c("name","desc","range", "minRange", "maxRange")),
                labelDescription=I(c("Name of Parameter", "Description of Parameter", "Range of Parameter", "Minimum Parameter Value after Transforamtion", "Maximum Parameter Value after Transformation"))
              )
  )
  
  ## Build the descripton
  txt <- list()
  txt["FCSversion"] <- "3"
  txt["$PAR"] <- as.character(npar)
  txt["$TOT"] <- as.character(dim(data)[1])
  txt["$BYTEORD"] <- "4,3,2,1"
  txt["$DATATYPE"] <- "F"
  txt["$MODE"] <- "L"
  txt["$NEXTDATA"] <- "0"
  
  txt["$BEGINANALYSIS"] <- "0"
  txt["$ENDANALYSIS"] <- "0"
  txt["$BEGINSTEXT"] <- "0"
  txt["$ENDSTEXT"] <- "0"
  dataStart <- 1000 ## This doesn't really matter, it's not an FCS file anyway, but let's provide something reasonable
  txt["$BEGINDATA"] <- as.character(dataStart)
  txt["$ENDDATA"] <- as.character(dataStart + npar * dim(data)[1] * 4 - 1) ## $DATATYPE is F and PnB is 32, so here we do: start + rows x cols x 4 bytes per value - 1
  
  txt[paste(id,"N",sep="")] <- names
  txt[paste(id,"S",sep="")] <- desc
  txt[paste(id,"R",sep="")] <- as.character(range)
  txt[paste(id,"B",sep="")] <- "32"
  txt[paste(id,"E",sep="")] <- "0,0"
  txt[paste(id,"G",sep="")] <- "1"
  txt[paste("flowCore_",id,"Rmin",sep="")] <- as.character(minRange)
  txt[paste("flowCore_",id,"Rmax",sep="")] <- as.character(maxRange)
  
  txt["$FIL"] <- basename(csvPath)
  txt["FILENAME"] <- csvPath
  txt["$SYS"] <- paste(Sys.info()['sysname'], Sys.info()['release'])
  txt["CREATOR"] <- R.version.string
  txt["transformation"] <- "applied"
  txt["GUID"] <- generateGUID(data)
  txt["ORIGINALGUID"] <- generateGUID(data)
  
  txt["$INST"] <- "Unknown institution"
  txt["$OP"] <- "Unknown operator"
  txt["$CYT"] <- "Unknown cytometer"
  txt["$SRC"] <- "Unknown source"
  
  curtime <- Sys.time()
  txt["$DATE"] <- toupper(format(curtime, "%d-%b-%Y")) ## Current data in the format of "05-OCT-2017"
  timeDelta <- 120 ## picking 2 minutes as cytometer run time
  if ("time" %in% tolower(names)) {
    txt["$TIMESTEP"] <- "0.01" ## We don't know that, so just something that would be reasonable-ish
    timeIndex <- which(tolower(names) == "time")
    timeDelta <- ceiling((max(data[,timeIndex]) - min(data[,timeIndex])) * 0.01)
  }
  if (toupper(format(curtime - timeDelta, "%d-%b-%Y")) == toupper(format(curtime, "%d-%b-%Y"))) {
    ## Subtracting timeDelta is still the same day, so let's do it that way.
    txt["$BTIM"] <- format(curtime - timeDelta, "%H:%M:%S")
    txt["$ETIM"] <- format(curtime, "%H:%M:%S")
  } else {
    ## Subtracting timeDelta gets us to yesterday, that's not good, so let's just say we started at midnight and add delta to it as ETIM...
    ## (all this is arbitrary anyway, we just want resonable values)
    txt["$BTIM"] <- "00:00:00"
    txt["$ETIM"] <- sprintf("%02i:%02i:%02i", floor(timeDelta / 3600), floor((timeDelta%%3600) / 60), floor(timeDelta%%60))
  }
  colnames(data) <- names
  list(fcs=new("flowFrame", exprs=as.matrix(data), parameters=pars, description=txt), cellIds=cellIds)
}
tmpRet = flowFrameFromFlowJoCSV(csvPath)
inputFCS <<- tmpRet$fcs
fsomParams <<- inputFCS@parameters@data$name #colnames(inputFCS)
#######################
## Half Violin Plot: ##
#######################

"%||%" <- function(a, b) {
  if (!is.null(a))
    a
  else
    b
}

geom_flat_violin <-
  function(mapping = NULL,
           data = NULL,
           stat = "ydensity",
           position = "dodge",
           trim = TRUE,
           scale = "area",
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomFlatViolin,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(trim = trim,
                    scale = scale,
                    ...)
    )
  }

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <-
  ggproto(
    "GeomFlatViolin",
    Geom,
    setup_data = function(data, params) {
      data$width <- data$width %||%
        params$width %||% (resolution(data$x, FALSE) * 0.9)
      
      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      data %>%
        dplyr::group_by(.data = ., group) %>%
        dplyr::mutate(
          .data = .,
          ymin = min(y),
          ymax = max(y),
          xmin = x,
          xmax = x + width / 2
        )
    },
    
    draw_group = function(data, panel_scales, coord)
    {
      # Find the points for the line to go all the way around
      data <- base::transform(data,
                              xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
      
      # Make sure it's sorted properly to draw the outline
      newdata <-
        base::rbind(
          dplyr::arrange(.data = base::transform(data, x = xminv), y),
          dplyr::arrange(.data = base::transform(data, x = xmaxv), -y)
        )
      
      # Close the polygon: set first and last point the same
      # Needed for coord_polar and such
      newdata <- rbind(newdata, newdata[1,])
      
      ggplot2:::ggname("geom_flat_violin",
                       GeomPolygon$draw_panel(newdata, panel_scales, coord))
    },
    
    draw_key = draw_key_polygon,
    
    default_aes = ggplot2::aes(
      weight = 1,
      colour = "grey20",
      fill = fColor,
      size = 0.5,
      alpha = NA,
      linetype = "solid"
    ),
    
    required_aes = c("x", "y")
  )
#######################
##   Robust Log:     ##
#######################
robustLog2 <- function(x) {
  x <- as.numeric(x)
  if (x >= 0) {
    log(x + 1, 2)
  } else {
    0
  }
}





