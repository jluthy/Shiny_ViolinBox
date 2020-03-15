##################################################
# Define server logic required to draw a heatmap #
##################################################
function(input, output) {
    # This is wrapped inside of reactive func to make it respond to user selection. 
    filterData <- reactive({
        # This takes input from Categorical Param Selector for group_by func
        groupedInput <- df %>%
            group_by_at(vars(input$catParam)) %>%
            summarise_all(median)
        # GI <- groupedInput[,paramNames == input$Parameters] 
        GI <- select(groupedInput, input$Parameters) # This is much easier to implement without having to remove unwanted columns from above code ex
        return(GI)
    })
    # This is wrapped inside of reactive func to make it respond to user selection. 
    filterData2 <- reactive({
        
        # Lets take the FlowMeans column and move it to the front of the DF before running gather()
        df2 <- df %>% select(input$catParam, everything())
        # Use gather function to quickly create the data frame. 
        violinPlotDF <- gather(df2, key = "Gene", value = "Log2Expression", 2:ncol(df2))
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
        violinPlotDF[input$catParam] <- as.factor(violinPlotDF[input$catParam])
        # GI <- groupedInput[,paramNames == input$Parameters] 
        # Keep the columns of interest
        VBP <- violinPlotDF[violinPlotDF$Gene == input$Parameters, ] # This is much easier to implement without having to remove unwanted columns from above code ex
        return(VBP)
    })
    # # Make a Categorical Parameter selector
    categoricalParam <- reactive({
        CP <- as.factor(paramNames[input$catParam])
        return(CP)
    })
    # the eventReactive will respond to the Refresh buttons in UI
    heatmap1 <- eventReactive(input$refreshPlot, {
        # Finally make a heatmap. assign this to an object or use later (not required). 
        pheatmap::pheatmap(filterData(), color = inferno(256))
        
        # TODO add more functionality with grouByCluster option.
    })
    output$heatmaps <- renderPlot(heatmap1())
    
    output$table1 <- renderDataTable(datatable(filterData()))
    # Raincloud Plots!  
    violinPlots <- eventReactive(input$refreshPlot, {
        ggplot(filterData2(),aes(x=Gene,y=Log2Expression, fill = Gene))+
            geom_flat_violin(position = position_nudge(x = .15, y = 0),adjust =2) +
            geom_point(alpha=0.05,position = position_jitter(width = .05, height=3), size = .25) +
            ylab('Score')+xlab('Group')+coord_flip()+theme_cowplot()+guides(fill = FALSE) +
            ggtitle('Figure 3: The Basic Raincloud with Colour')
    })
    output$violins <- renderPlot(violinPlots())
    
}