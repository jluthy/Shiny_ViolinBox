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

# Choose from the two available csv files in extData folder
inCSVPath <- file.choose()  # "/Users/10273835/Documents/code/exTidyData/B1.smallPop.catPar.ExtNode.csv"
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

hmPalette <- c("magma", "plasma", "inferno", "viridis", "cividis", "rainbow")
vbPalette <- c("Dark2","Accent","Pastel2","Set2", "Pastel1", "Set1","Paired","Set3","Spectral","BrBG","PiYG","PRGn","PuOr")


# Quantial Color scale
quantile_breaks <- function(xs, n = 10) {
  breaks <- quantile(xs, probs = seq(0, 1, length.out = n))
  breaks[!duplicated(breaks)]
}
df2 <- as.matrix(df)
mat_breaks <- quantile_breaks(df2, n = 11)
# plot(mat_breaks)
# mat_breaks

##      CREATE FUNCTIONS     ##
###############################
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





