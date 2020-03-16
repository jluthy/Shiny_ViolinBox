##################################################
# Define UI for application that draws a heatmap #
##################################################
fluidPage(
    
    # Application title
    titlePanel("Sample Data Table and Plots"),
    
    # Sidebar with a select input for the parameters to heatmap
    sidebarLayout(
        sidebarPanel(
            selectInput("Parameters",
                        label = "Select Parameters to Heatmap",
                        choices = paramNames,
                        multiple = TRUE,
                        selectize = TRUE),
            selectInput("catParam",
                        label = "Select a Categorical Parameter",
                        choices = c("None", as.character(paramNames)),
                        selected = NULL),
            actionButton("refreshPlot", 
                         label = "Refresh Plots")
        ),
        
        # Show a plot of the generated table, heatmap, and raincloud plots.
        mainPanel(
            DTOutput("table1"),
            plotOutput("heatmaps"),
            plotOutput("violins")
        )
    )
)
