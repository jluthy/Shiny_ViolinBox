##################################################
# Define server logic required to draw a heatmap #
##################################################
function(input, output, session) {
  
  session_store <- reactiveValues()
  
  #################################
  # Create the Data Table         #
  #################################
  # This is wrapped inside of reactive func to make it respond to user selection. 
  filterDT <- reactive({
    DTdf <- select(df, input$DTParameters)
    return(DTdf)
  })
  output$table1 <- renderDataTable(datatable(filterDT()))
  #################################
  # Create the Heatmap Data Frame #
  #################################
  # This is wrapped inside of reactive func to make it respond to user selection. 
  # This will return the df for heatmaps
  filterData <- reactive({
    if (input$hmCatParam == "None"){
      HMdf <- select(df, input$hmParameters)
      return(HMdf)
    }else{
      # This takes input from Categorical Param Selector for group_by func
      groupedInput <- df %>%
        group_by_at(vars(input$hmCatParam)) %>%
        summarise_all(median)
      # GI <- groupedInput[,paramNames == input$Parameters] 
      GI <- select(groupedInput, input$hmParameters) # This is much easier to implement without having to remove unwanted columns from above code ex
      return(GI)
    }
    
  })
  ###################################
  # Create the Raincloud Data Frame #
  ###################################
  # This is wrapped inside of reactive func to make it respond to user selection. 
  # This will return the df used for Violin Plots
  filterData2 <- reactive({
    if (input$catParam == "None") {
      print("no Categorical selected")
      # dfnoCat <- df
      dfnoCat <- select(df, input$Parameters)
      # Use gather function to quickly create the data frame.
      violinPlotDF <- gather(dfnoCat, key = "Gene", value = "Log2Expression", 1:ncol(dfnoCat))
      tempDF <- sapply(violinPlotDF$Log2Expression, robustLog2)
      tempDF <- data.frame(matrix(unlist(tempDF), nrow=length(tempDF), byrow=T))
      violinPlotDF <- cbind(violinPlotDF, tempDF)
      violinPlotDF <-  violinPlotDF[,-2]
      colnames(violinPlotDF)[2] <- "Log2Expression"
      
      violinPlotDF$Log2Expression <- as.numeric(violinPlotDF$Log2Expression)
      violinPlotDF$Gene <- as.factor(violinPlotDF$Gene)
      VBPnoCat <- violinPlotDF[violinPlotDF$Gene == input$Parameters, ]
      return(VBPnoCat)
    }else{
      print("catParam Selected")
      # Lets take the catParam column and move it to the front of the DF before running gather()
      df01 <- select(df, c(input$Parameters,input$catParam))
      dfCat2 <- df %>% select(input$catParam, everything())
      # if (isSeqGeq == F) {
      #     dfCat2$EventNumberDP <- NULL
      # }
      # Use gather function to quickly create the data frame. 
      violinPlotDF <- gather(dfCat2, key = "Gene", value = "Log2Expression", 2:ncol(dfCat2))
      if(ncol(violinPlotDF) < 3){
        print("Try again")
        stopAndReturnError("Unable to read the inputs correctly, check parameter names.", call. = FALSE)
      }
      # can use sapply to apply the robustlog2 to Values
      tempDF <- sapply(violinPlotDF$Log2Expression, robustLog2)
      tempDF <- data.frame(matrix(unlist(tempDF), nrow=length(tempDF), byrow=T))
      violinPlotDF <- cbind(violinPlotDF, tempDF)
      violinPlotDF <-  violinPlotDF[,-3]
      colnames(violinPlotDF)[3] <- "Log2Expression"
      rm(tempDF)
      
      # set up the DF for plotting
      violinPlotDF$Log2Expression <- as.numeric(violinPlotDF$Log2Expression)
      violinPlotDF$Gene <- as.factor(violinPlotDF$Gene)
      violinPlotDF[input$catParam] <- as.factor(gsub("\\s*", "", as.matrix(violinPlotDF[input$catParam])))
      
      if (input$grpByParam == TRUE && input$overlay == F) {
        # if(input$overlay == F) {
        colnames(violinPlotDF) <- c("Gene", as.character(input$catParam), "Log2Expression")
        # } else {
        VBP <- violinPlotDF[violinPlotDF[[input$catParam]] == input$Parameters, ]
        # }
      }else{
        VBP <- violinPlotDF[violinPlotDF$Gene == input$Parameters, ]
      }
      return(VBP)
    }
    
  })
  ##################################
  # Create the BeeSwarm Data Frame #
  ##################################
  filterDataBS <- reactive({
    if (input$bscatParam == "None"){
      # dfnoCat <- df
      print("Beeswarm no catPar")
      swarmNoCat <- select(df, everything(input$bsParameters))
      # Use gather function to quickly create the data frame.
      swarmNoCat <- gather(swarmNoCat, key = "Gene", value = "Log2Expression", 1:ncol(swarmNoCat))
      tempDF <- sapply(swarmNoCat$Log2Expression, robustLog2)
      tempDF <- data.frame(matrix(unlist(tempDF), nrow=length(tempDF), byrow=T))
      swarmNoCat <- cbind(swarmNoCat, tempDF)
      swarmNoCat <-  swarmNoCat[,-2]
      colnames(swarmNoCat)[2] <- "Log2Expression"
      
      swarmNoCat$Log2Expression <- as.numeric(swarmNoCat$Log2Expression)
      swarmNoCat$Gene <- as.factor(swarmNoCat$Gene)
      BswarmDF <<- swarmNoCat[swarmNoCat$Gene == input$bsParameters, ]
      # labels <<- BswarmDF$Gene
      return(BswarmDF)
      
    }
    # else{
    #     swarmCat <- df %>% select(input$bscatParam, everything())
    #     # Use gather function to quickly create the data frame. 
    #     swarmCat <- gather(swarmCat, key = "Gene", value = "Log2Expression", 2:ncol(swarmCat))
    #     if(ncol(swarmCat) < 3){
    #         print("Try again")
    #         stopAndReturnError("Unable to read the inputs correctly, check parameter names.", call. = FALSE)
    #     }
    #     # can use sapply to apply the robustlog2 to Values
    #     tempDF <- sapply(swarmCat$Log2Expression, robustLog2)
    #     tempDF <- data.frame(matrix(unlist(tempDF), nrow=length(tempDF), byrow=T))
    #     swarmCat <<- cbind(swarmCat, tempDF)
    #     swarmCat <-  swarmCat[,-3]
    #     colnames(swarmCat)[3] <- "Log2Expression"
    #     rm(tempDF)
    #     
    #     # set up the DF for plotting
    #     swarmCat$Log2Expression <- as.numeric(swarmCat$Log2Expression)
    #     swarmCat$Gene <- as.factor(swarmCat$Gene)
    #     swarmCat[input$bscatParam] <- as.factor(gsub("\\s*", "", as.matrix(swarmCat[input$bscatParam])))
    #     BswarmDF2 <<- swarmCat[swarmCat$Gene == input$Parameters, ]
    #     # labels <<- BswarmDF2$Gene
    #     return(BswarmDF2) 
    # }
    
  })
  ##################################
  # Create the FlowSOM Data Subset #
  ##################################
  flowSomFcs <- reactive({
    print("did we select fcs params?")
    tmpRet = flowFrameFromFlowJoCSV(csvPath)
    inputFCS <- tmpRet$fcs
    fsomParams <- colnames(inputFCS@exprs)
    # inputFCStest <- inputFCS[ ,inputFCS@exprs == input$flowsomParams]
    return(inputFCS)
  })
  
  ##################################
  # Create the FlowSOM Plot #
  ##################################
  flowsom1 <- eventReactive(input$FSrefreshPlot, {
    print("made it to refreshing FlowSOM")
    set.seed(input$seedSlider)
    flowsom.res <- FlowSOM::FlowSOM(flowSomFcs(), compensate=FALSE,transform=TRUE, toTransform = input$flowsomParams,
                                    scale=TRUE,colsToUse=input$flowsomParams,nClus = NULL, maxMeta = 20) 
    # Have a look at the resulting tree
    FlowSOM::PlotStars(flowsom.res[[1]],backgroundValues = as.factor(flowsom.res[[2]]),
                       colorPalette = grDevices::colorRampPalette(heat.colors(n=7,alpha=0.7)) ,
                       backgroundColor = hcl.colors(n=10, palette = input$somPalette, alpha = 0.6 ))
  })
  output$flowsomPlot <- renderPlot(flowsom1())
  
  # filterDataSOM <- reactive({
  #     flowSOM.res <- FlowSOM::FlowSOM(inputFCS, compensate=FALSE,transform=TRUE, toTransform = c(4,5,6,7,8,9),
  #                                     scale=TRUE,colsToUse=c(4,6,8,9,10,11,12,13),nClus = 8, maxMeta = NULL)
  #     # Have a look at the resulting tree
  #     FlowSOM::PlotStars(flowSOM.res[[1]],backgroundValues = as.factor(flowSOM.res[[2]]),
  #                        colorPalette = grDevices::colorRampPalette(heat.colors(n=7,alpha=0.7)) ,
  #                        backgroundColor = hcl.colors(n=10, palette = "reds", alpha = 0.6 ))
  # })
  ######################################
  # Create the Refresh Button HEatmap #
  ######################################
  # the eventReactive will respond to the Refresh buttons in UI
  heatmap1 <- eventReactive(input$HMrefreshPlot, {
    print("so fresh")
    if(input$hmCatParam == "None"){
      if(input$scaleHM){
        scl = "column"
      }else{
        scl = "none"
      }
      if(input$dendrogram){
        clusterD = T
      }else{
        clusterD = F
      }
      #     plot_ly(filterData(),
      #                     x=colnames(filterData()),
      #                     y=rownames(filterData()),
      #                     type = "heatmap",
      #                     colorscale = "viridis") %>% 
      #         layout(xaxis=list(
      #                 title="HeatMap"),
      #                yaxis=list(
      #                 title="Something Nice",
      #                 
      #                )
      #         )
      pheatmap::pheatmap(filterData(),color = eval(parse(text = input$hmPalette))(n=length(mat_breaks)-1),
                         border_color = "grey20",
                         main = "",
                         show_rownames = TRUE,
                         show_colnames = TRUE,
                         fontsize_col = 8,
                         angle_col = "45",
                         fontsize_row = 8,
                         kmeans_k = 30,
                         cluster_rows = clusterD,
                         cluster_cols = clusterD)
    }else{
      # Finally make a heatmap. assign this to an object or use later (not required).
      pheatmap::pheatmap(filterData(), color = eval(parse(text = input$hmPalette))(n=length(mat_breaks)-1),
                         border_color = "grey20",
                         main = "",
                         show_rownames = TRUE,
                         show_colnames = TRUE,
                         fontsize_col = 8,
                         angle_col = "45",
                         fontsize_row = 8,
                         cluster_rows = clusterD,
                         cluster_cols = clusterD)
    }
  })
  output$heatmaps1 <- renderPlot(heatmap1())
  ##################################
  ## Create the BeeSwarm Refresh  ##
  ##################################
  beeswarmz1 <- eventReactive(input$BSrefreshPlot, {
    print("made it to beeswarm refresh plot")
    # beeswarm(input$bsParameters ~ input$bscatParam,
    #          data = filterDataBS(),
    #          method = "swarm",
    #          main = "boxplot + beeswarm",
    #          col = sample(colors(),10),
    #          cex = .33,
    #          pch = 18,
    #          priority = "density")
    # colorLength <- unique(filterDataBS()[,1])
    labels <- filterDataBS()[,1]
    # lencolors <- length(unique(BswarmDF2$Gene))
    ggplot(mapping = aes(labels,
                         data = filterDataBS()[,2],
                         color=labels)) +
      geom_quasirandom(aes(y= filterDataBS()[,2],
                           x= labels),
                       method = 'smiley',
                       nbins = 95,
                       varwidth = TRUE,
                       alpha=.4,
                       shape = 2) 
    
    # ggplot(mapping = aes(labels,
    #                      input$bsParameters)) +
    #     geom_quasirandom(method = 'smiley',
    #                      # nbins = 50,
    #                      width = .33,
    #                      dodge.width = .9,
    #                      varwidth = TRUE,
    #                      alpha=.2)
  })
  output$beeswarmPlot <- renderPlot(beeswarmz1())
  # output$table2 <- renderDataTable(datatable(filterData()))
  ####################################
  ## Create the Rainclouds Refresh  ##
  ####################################
  # Raincloud Plots!  
  violinPlots <- eventReactive(input$refreshPlot, {
    print("made it to ViolinPlotting...please stand by")
    # Set the plot options
    if(input$flipViolins == TRUE){
      parFlipGraph <- TRUE
    }else{
      parFlipGraph <- FALSE
    }
    if(input$darkTheme == TRUE){
      parDarkTheme <- TRUE
    }else{
      parDarkTheme <- FALSE
    }
    if(input$jitter == TRUE){
      parJitter <- TRUE
    }else{
      parJitter <- FALSE
    }
    if(input$violins == TRUE){
      parViolin <- TRUE
    }else{
      parViolin <- FALSE
    }
    if(input$boxes == TRUE){
      parBox <- TRUE
    }else{
      parBox <- FALSE
    }
    if(input$overlay == TRUE){
      parOverlayPlots <- TRUE
    }else{
      parOverlayPlots <- FALSE
    }
    # if (input$grpByParam == TRUE){
    #     numUniqueParams <- length(unique(filterData2()[,1]))
    # } else{
    #     numUniqueParams <- length(unique(filterData2()[,2]))
    # }
    numUniqueParams <- length(unique(filterData2()[,1])) + length(unique(filterData2()[,2]))
    ## This color palette generator requires a minimum of 4 parameters, so:
    if (numUniqueParams < 6) {
      numUniqueParams <- 6
    }
    getPalette <- colorRampPalette(brewer.pal(8, input$vbPalette))(numUniqueParams)
    scaleShade <- scale_colour_manual(values = getPalette)
    scaleFill <-  scale_fill_manual(values = getPalette)
    ylabel <-	ylab('Log2Expression')
    xlabel <-  xlab('xLabel')
    
    #######################
    ## Make it Rain:     ##
    #######################
    makeItRain <- function(p) {
      if(parViolin && parJitter && parBox){
        print("All the graphix!")
        p <- p + violinPart + jitterPart + boxPart +
          scaleShade + scaleFill + xlabel + ylabel + guide
      }else if(parViolin && parBox && !parJitter){
        print("Weather calls for boxy clouds")
        p <- p + violinPart + boxPart +
          scaleShade + scaleFill + guide + xlabel + ylabel
      }else if(parViolin && parJitter && !parBox){
        print("Jittery Violins - aka Staccato")
        p <- p + violinPart + jitterPart +
          scaleShade + scaleFill + guide + xlabel + ylabel
      }else if(parViolin && !parBox && !parJitter){
        print("Violin Solo")
        p <- p + violinSoloPart +
          scaleShade + scaleFill + guide + xlabel + ylabel
      }else if(parJitter && parBox && !parViolin) {
        print("Box and Whisker with a bit of Jitter")
        p <- p + jitterPart + boxPart +
          scaleShade + scaleFill + guide + xlabel + ylabel
      }else if(parJitter && !parViolin && !parBox){
        print("Jitter Only?! Go Home.")
        p <- p + jitterPart +
          scaleShade + scaleFill + guide + xlabel + ylabel
      }else if(parBox && !parJitter && !parViolin){
        print("Box and Whisker Only")
        p <- p + boxPart +
          scaleShade + scaleFill + guide + xlabel + ylabel
      }else{
        print("NO GRAPH OPTIONS SELECTED")
        p <- p +
          scaleShade + scaleFill + guide + xlabel + ylabel
      }
      return (p)
    }
    
    ###############################################
    # Setting Default Variables for 'Nicer' Plots #
    ###############################################
    rainGap <- 0.1
    rainDropSize <- 2.2
    jitterWidth <- 0.05
    jitterHeight <-1.2
    boxWidth <- 0.2
    boxNudgeX <- -0.12
    boxNudgeY <- 0
    boxDodgeW <- 0.2
    violinWidth <- 1.2
    # rainGap <- 0.15
    # TODO make these below into boolean flags/buttons/checkbox for user
    
    if(parDarkTheme){
      jColor <- "WHITE"
      fColor <- "BLACK"
      lColor <- "white"
    } else if (!parDarkTheme) {
      jColor <- "BLACK"
      fColor <- "WHITE"
      lColor <- "black"
    }
    
    if (parFlipGraph) {
      flipAngle <- 0
      theme01 <- theme(text = element_text(size = input$bigfonts),
                       axis.text.x = element_text(angle = 0,vjust = 0.5, hjust = 0.95, size = input$smallfonts, colour = jColor),
                       axis.text.y = element_text(size = input$smallfonts, colour = jColor),
                       strip.text = element_text(size = input$tilefonts),
                       legend.text = element_text(size = input$legendfonts, colour = jColor),
                       legend.key = element_rect(fill = fColor, colour = NULL),
                       axis.title.x = element_text(colour = jColor, size = input$bigfonts),
                       axis.title.y = element_text(colour = jColor, size = input$bigfonts)
      )
    }else{
      flipAngle <- 90
      theme01 <- theme(
        text = element_text(size = input$bigfonts),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95, size = input$smallfonts, colour = jColor),
        axis.text.y = element_text(size = input$smallfonts, colour = jColor),
        strip.text = element_text(size = input$tilefonts),
        legend.text = element_text(size = input$legendfonts, colour = jColor),
        legend.key = element_rect(fill = fColor, colour = NULL),
        axis.title.x = element_text(colour = jColor, size = input$bigfonts),
        axis.title.y = element_text(colour = jColor, size = input$bigfonts))
    }
    
    p <- ggplot(filterData2(), aes(x = Gene, y = Log2Expression, colour = Gene), text)
    
    ##########################
    ##     Plot Types:      ##
    ##########################
    if (input$catParam != "None") {
      fillerParam <- filterData2()[[input$catParam]]
      AES <- aes(fill = fillerParam)
    } else {
      fillerParam <- filterData2()[[1]]
      AES <- aes(fill = fillerParam, colour = fillerParam)
      
    }
    ## Note: We create the overlay in these plots!
    violinSoloPart <- geom_violin(AES, position = position_nudge(x = .2, y = 0),
                                  adjust = 2, trim = FALSE,alpha = .5,
                                  colour = NA,width = 1.2)
    
    violinPart <- geom_flat_violin(AES,position = position_nudge(x = rainGap, y = 0),
                                   adjust = 2,trim = FALSE,alpha = .5,
                                   colour = NA,width = violinWidth)
    
    if (parJitter && parBox && !parViolin) {
      jitterPart <- geom_jitter(aes(x = Gene, y = Log2Expression,fill = fillerParam),
                                position = position_jitter(width = jitterWidth),
                                size = rainDropSize,
                                alpha = 0.1,
                                colour = jColor,
                                size = 0.1)
    } else if (parJitter && !parBox && !parViolin) {
      jitterPart <- geom_point(aes(x = Gene,y = Log2Expression,fill = fillerParam),
                               alpha = .1,
                               position = position_jitter(width = jitterWidth, height = jitterHeight),
                               size = rainDropSize,
                               shape = 21,
                               alpha = 0.1,
                               colour = jColor,
                               size = 0.1
      )
    } else{
      jitterPart <-geom_point( aes(
        # x = as.numeric(Gene) - .15,
        x = Gene,
        y = Log2Expression,
        fill = fillerParam
      ),
      alpha = .1,
      position = position_jitter(width = jitterWidth, height = jitterHeight),
      size = rainDropSize,
      shape = 21,
      alpha = 0.1,
      colour = jColor,
      size = 0.1
      )
    }
    if (parJitter && parBox && !parViolin) {
      boxPart <- geom_boxplot(
        aes(
          x = Gene,
          y = Log2Expression,
          fill = fillerParam
        ),
        outlier.colour = NA,
        width = boxWidth,
        position = position_dodge(preserve = "total", width = boxDodgeW),
        colour = jColor,
        size = 0.1
      )
    } else{
      boxPart <-
        geom_boxplot(
          aes(
            x = Gene,
            y = Log2Expression,
            fill = fillerParam
          ),
          position = position_dodge(preserve = "total", width = boxDodgeW),
          outlier.shape = NA,
          width = boxWidth,
          colour = jColor,
          size = 0.1
        )
    }
    
    # misc:
    if(input$grpByParam){
      xLabel = "Clusters"
    } else {
      xLabel <- "Parameters"
    }
    
    ylabel <-	ylab('Log2Expression')
    xlabel <-  xlab(xLabel)
    
    ttle <- as.character(input$catParam)
    if(input$grpByParam == T){
      ttle <- "Parameter"
      if(input$overlay){
        ttle <- as.character(input$catParam)
      }
    }
    
    # if (input$addLegend){
    #   guide <- guides(fill=guide_legend(title=ttle))
    # } else {
    #   guide <- guides(fill = FALSE, colour = FALSE)
    # }
    # theme <- theme_cowplot()
    
    ###  TODO - Add Stat Comparisons:
    ##     * Remove non-statistically significant comparisons
    #################################
    ## Add Statistical Comparisons ##
    #################################
    if (input$statCompare) {
      maximus <- max(filterData2()[[3]])
      minimus <- min(filterData2()[[3]])
      factor <- "Gene"
      groups <- as.character(unique(filterData2()[[factor]]))
      expand.grid.unique <- function(x, y, include.equals=FALSE){
        x <- unique(x)
        y <- unique(y)
        g <- function(i){
          z <- setdiff(y, x[seq_len(i-include.equals)])
          if(length(z)) cbind(x[i], z, deparse.level=0)
        }
        do.call(rbind, lapply(seq_along(x), g))
      }
      
      combs <- as.data.frame(expand.grid.unique(groups, groups), stringsAsFactors=FALSE)
      my.comps <- as.data.frame(t(combs), stringsAsFactors=FALSE)
      colnames(my.comps) <- NULL
      rownames(my.comps) <- NULL
      my_comparisons <- as.list(my.comps)
      height.y = c((maximus+0.05*maximus), (maximus+0.11*maximus), (maximus+0.17*maximus))
      symn <- list(cutpoints = c(0, 0.0001, 0.001, 0.01), symbols = c("****", "***", "**", "*"))
      # symn <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05), symbols = c("****", "***", "**", "*"))
      # if (!parOverlayPlots) {
      if (!parFlipGraph) {
        p <- p + stat_compare_means(comparisons = my_comparisons,
                                    size=3,
                                    label = "p.signif",
                                    hide.ns = TRUE,
                                    symnum.args <- symn,
                                    na.rm = TRUE,
                                    step.increase = 0.2,
                                    show.legend = F,
                                    inherit.aes = T,
                                    color = jColor)
      }
      if (input$catParam != "None" ) {
        
        ## Adjust the position of the KW value
        xkw <- 1.4
        if (!parOverlayPlots) {
          ykw <- minimus-0.55*maximus
        } else {
          # ykw <- minimus+0.01*maximus
          ykw <- minimus-0.80*maximus
          xkw <- 0.8
        }
        
        guide <- guides(fill = FALSE, colour = FALSE)
        ## only run KW test if there's a categorical parameter:
        p <- p + ggpubr::stat_compare_means(label.x = xkw,
                                            label.y = (ykw),
                                            size=2.3,
                                            colour = jColor,
                                            show.legend = F,
                                            inherit.aes = T)
        ## With a categorical parameter the y-axis max spacing needs some help:
        p <- p + scale_y_continuous(expand = expand_scale(mult = c(.1, .2)))
        
        guide <- guides(fill = FALSE, colour = FALSE)
        
      }
      
      guide <- guides(fill = FALSE, colour = FALSE)
    }
    
    if (input$addLegend){
      guide <- guides(fill=guide_legend(title=ttle))     ## , colour = element_text(colour = lColor))
    } else {
      guide <- guides(fill = FALSE, colour = FALSE)
    }
    theme <- theme_cowplot()
    
    #################################
    ##   Put the Plots Together:   ##
    #################################
    if(!parOverlayPlots && (input$catParam != "None")){
      p <- p + facet_wrap(~as.factor(.data[[as.character(input$catParam)]]))
    }
    p <- makeItRain(p)
    p <- p + theme + theme01
    
    # if(input$catParam != "None"){
    #   p <- p + facet_wrap(~as.factor(.data[[as.character(input$catParam)]]))
    # }
    
    if (parFlipGraph) {
      p <- p + coord_flip()
    }
    if (parDarkTheme) {
      p <- p + theme_dark() + theme(axis.text.x = element_text(angle = flipAngle, vjust = 0.5, hjust = 0.95, size = 15, colour = jColor),
                                    axis.text.y = element_text(size = 15, colour = jColor),
                                    strip.text = element_text(size = 20),
                                    plot.background = element_rect(fill = fColor),
                                    legend.text = element_text(colour = jColor),
                                    legend.background = element_rect(fill = fColor),
                                    panel.background = element_rect(fill = fColor),
                                    legend.key = element_rect(fill = fColor, colour = NULL))
    }
    return(p)
  })
  
  output$print <- downloadHandler(
    filename = function() {
      paste("ViolinBox.html", sep = "")
      # paste("File.html", sep = "")
    },
    content = function(file) {
      saveWidget(as_widget(session_store$plt, file, selfcontained = T))
    }
  )
  
  output$violins <- renderPlot(violinPlots())
  
}
