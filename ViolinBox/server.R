##################################################
# Define server logic required to draw a heatmap #
##################################################
function(input, output) {
    # TODO add booleans from checkboxgroup to feed into violin plots
    # rv <- reactiveValues(a=FALSE, b=FALSE)
    # 
    # observe( {
    #     is.a <- 'parDarkTheme' %in% input$violinCheckGroup
    #     if (  rv$a != is.a){
    #         rv$a <- is.a
    #     }
    #     is.b <- 'parFlipGraph' %in% input$violinCheckGroup
    #     if (  rv$b != is.b){
    #         rv$b <- is.b
    #     }
    # })
    # 
    # observeEvent(rv$a, {
    #     print("a only")
    # })
    # observeEvent(input$a, {
    #     print("I'm here (a only)")
    # })
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
    # This is wrapped inside of reactive func to make it respond to user selection. 
    # This will return the df used for Violin Plots
    filterData2 <- reactive({
        if (input$catParam == "None") {
            
            dfnoCat <- df
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
            # Lets take the catParam column and move it to the front of the DF before running gather()
            dfCat2 <- df %>% select(input$catParam, everything())
            if (isSeqGeq == F) {
                dfCat2$EventNumberDP <- NULL
            }
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
            
            if (input$grpByParam == TRUE) {
                violinPlotDF.saveGame <- violinPlotDF
                colnames(violinPlotDF) <- c("Gene", as.character(input$catParam), "Log2Expression")
                VBP <- violinPlotDF[violinPlotDF[[input$catParam]] == input$Parameters, ]
                # ttle <- "Parameter"
                # theme01 <- theme02
            }else{
                VBP <- violinPlotDF[violinPlotDF$Gene == input$Parameters, ]
            }
            return(VBP)
        }
        
    })
    # # Make a Categorical Parameter selector
    # categoricalParam <- reactive({
    #     # Lets take the catParam column and move it to the front of the DF before running gather()
    #     dfCat2 <- df %>% select(input$catParam, everything())
    #     # Use gather function to quickly create the data frame. 
    #     violinPlotDF <- gather(dfCat2, key = "Gene", value = "Log2Expression", 2:ncol(dfCat2))
    #     # can use sapply to apply the robustlog2 to Values
    #     tempDF <- sapply(violinPlotDF$Log2Expression, robustLog2)
    #     tempDF <- data.frame(matrix(unlist(tempDF), nrow=length(tempDF), byrow=T))
    #     violinPlotDF <- cbind(violinPlotDF, tempDF)
    #     violinPlotDF <-  violinPlotDF[,-3]
    #     colnames(violinPlotDF)[3] <- "Log2Expression"
    #     rm(tempDF)
    #     CP <- violinPlotDF[input$catParam] <- as.factor(violinPlotDF[input$catParam])
    #     
    #     return(CP)
    # })
    # the eventReactive will respond to the Refresh buttons in UI
    heatmap1 <- eventReactive(input$HMrefreshPlot, {
        if(input$catParam == "None"){
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
                                     breaks = mat_breaks,
                                     border_color = "grey20",
                                     main = "",
                                     show_rownames = FALSE,
                                     show_colnames = TRUE,
                                     fontsize_col = 8,
                                     angle_col = "45",
                                     fontsize_row = 8,
                                     kmeans_k = 30)
        }else{
            # Finally make a heatmap. assign this to an object or use later (not required).
            pheatmap::pheatmap(filterData(), color = eval(parse(text = input$hmPalette))(n=length(mat_breaks)-1),
                               border_color = "grey20",
                               main = "",
                               show_rownames = FALSE,
                               show_colnames = TRUE,
                               fontsize_col = 8,
                               angle_col = "45",
                               fontsize_row = 8)
        }
    })
    
    output$heatmaps1 <- renderPlot(heatmap1(),
                                   width = "auto")
    
    output$table1 <- renderDataTable(datatable(filterData()))
    
    # output$table2 <- renderDataTable(datatable(filterData()))
    
    # Raincloud Plots!  
    violinPlots <- eventReactive(input$refreshPlot, {
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
        
        if (input$grpByParam == TRUE){
            numUniqueParams <- length(unique(filterData2()[,1]))
        } else{
            numUniqueParams <- length(unique(filterData2()[,2]))
        }
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
        rainDropSize <- 2.5
        jitterWidth <- 0.05
        jitterHeight <-1.2
        boxWidth <- 0.2
        boxNudgeX <- -0.12
        boxNudgeY <- 0
        boxDodgeW <- 0.2
        violinWidth <- 1.2
        rainGap <- 0.15
        # TODO make these below into boolean flags/buttons/checkbox for user

        
        if(parDarkTheme){
            jColor <- "WHITE"
            fColor <- "BLACK"
        } else {
            jColor <- "BLACK"
            fColor <- "WHITE"
        }
        # if (groupByClust) {
        #     xLabel <- paste0(input$catParam, " Clusters")
        #     theme02 <- theme(axis.text.x = element_text(angle = 48, hjust = 1.1, vjust = 1, size = 15, colour = jColor),
        #                      axis.text.y = element_text(size = 15, colour = jColor),
        #                      strip.text = element_text(size = 20),
        #                      legend.text = element_text(size = 15, colour = jColor),
        #                      legend.key = element_rect(fill = fColor, colour = NULL))
        # }
        # 
        if (parFlipGraph) {
            theme01 <- theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size = 15, colour = jColor),
                             axis.text.y = element_text(size = 15, colour = jColor),
                             strip.text = element_text(size = 20),
                             legend.text = element_text(size = 15, colour = jColor),
                             legend.key = element_rect(fill = fColor, colour = NULL))
        }else{
            theme01 <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 15, colour = jColor),
                             axis.text.y = element_text(size = 15, colour = jColor),
                             strip.text = element_text(size = 20),
                             legend.text = element_text(size = 15, colour = jColor),
                             legend.key = element_rect(fill = fColor, colour = NULL))
        }
        
        p <- ggplot(filterData2(),aes(x=Gene,y=Log2Expression, fill = Gene, colour = Gene), text)
        violinSoloPart <- geom_violin(position = position_nudge(x = .1, y = 0),
                                      adjust = 2.5,
                                      trim = FALSE,
                                      width = 1.2)
        
        violinPart <- geom_flat_violin(position = position_nudge(x = rainGap, y = 0),
                                       adjust = 2.5,
                                       trim = FALSE,
                                       width = violinWidth)
        
        if (parJitter && parBox && !parViolin) {
            jitterPart <- geom_jitter(aes(x = Gene,y = Log2Expression),
                                      position = position_jitter(width = jitterWidth),
                                      alpha = 0.1,
                                      colour = jColor)
            
        }else{
            jitterPart <- geom_point(alpha = .10,
                                     position = position_jitter(width = jitterWidth,
                                                                height = jitterHeight),
                                     size = rainDropSize)
        }
        
        if (parJitter && parBox && !parViolin) {
            boxPart <- geom_boxplot(aes(x = Gene,
                                        y = Log2Expression),
                                    outlier.colour = NA,
                                    width = boxWidth,
                                    position = position_dodge(preserve = "total",width = boxDodgeW),
                                    colour = jColor)
            
        }else{
            boxPart <- geom_boxplot(aes(x = Gene,
                                        y = Log2Expression),
                                    position = position_nudge(x = boxNudgeX,y = boxNudgeY),
                                    outlier.shape = NA,
                                    alpha = 0.3,
                                    width = boxWidth,
                                    colour = jColor)
        }
        if(input$grpByParam){
            xLabel = "Clusters"
        } else {
            xLabel <- "Parameters"
        }
 
        # misc:
        ylabel <-	ylab('Log2Expression')
        xlabel <-  xlab(xLabel)
        theme <- theme_cowplot()
        guide <- guides(fill = FALSE, colour = FALSE)
        p <- makeItRain(p)
        p <- p + theme + theme01
        
        if(parOverlayPlots){
              p <- p + facet_wrap(.data[[facetParam]])
            }
        
        if (parFlipGraph) {
            p <- p + coord_flip()
        }
        
        if (parDarkTheme) {
            p <- p + theme_dark() + theme(axis.text.x = element_text(size = 15, colour = jColor),
                                          axis.text.y = element_text(size = 15, colour = jColor),
                                          strip.text = element_text(size = 20),
                                          plot.background = element_rect(fill = fColor),
                                          legend.background = element_rect(fill = fColor),
                                          panel.background = element_rect(fill = fColor),
                                          legend.key = element_rect(fill = fColor, colour = NULL))
        }
        return(p)
    })
    output$violins <- renderPlot(violinPlots())
    
}
