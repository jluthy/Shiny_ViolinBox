##################################################
# Define UI for application that draws a heatmap #
##################################################
fluidPage(
    
    # Application title
    titlePanel("ViolinBox Plugin Proto"),
    
    # Sidebar with a select input for the parameters to heatmap
    sidebarLayout(
        sidebarPanel(
            fileInput("file", h3("CSV File input")),
            selectInput("Parameters",
                        label = "Select Parameters to Heatmap",
                        choices = as.character(paramNames),
                        multiple = TRUE,
                        selectize = TRUE),
            selectInput("catParam",
                        label = "Select a Categorical Parameter",
                        choices = c("None", as.character(paramNames)),
                        selected = NULL),
            checkboxInput("flipViolins",
                          label = "Flip Violins",
                          FALSE),
            checkboxInput("darkTheme",
                          label = "Dark Theme",
                          FALSE),
            checkboxInput("jitter",
                          label = "Jitter",
                          FALSE),
            checkboxInput("violins",
                          label = "Violins",
                          TRUE),
            checkboxInput("boxes",
                          label = "Box-n-Whisker",
                          TRUE),
            # checkboxInput("grpClust",
            #               label = "Group By Cluster",
            #               FALSE),
            # checkboxGroupInput("violinCheckGroup",
            #                    label = h3("Violin Plot Options"),
            #                    choices = list("Group by Cluster" = 1,
            #                                   "Flip Violins" = "parFlipGraph",
            #                                   "Dark Theme" = "parDarkTheme",
            #                                   "Add Violins" = 4,
            #                                   "Add Jitter" = 5,
            #                                   "Add Boxes" = 6
            #                                   ),
            #                    selected = TRUE),
            actionButton("refreshPlot", 
                         label = "Refresh Plots")
        ),
        
        # Show a plot of the generated table, heatmap, and raincloud plots.
        mainPanel(
            tabsetPanel(
                tabPanel("Heatmap",
                         fluidRow(
                            column(12, DTOutput("table1")),
                            column(12, plotOutput("heatmaps"))
                         )),
                tabPanel("Violin Plots",
                         fluidRow(
                             column(12, DTOutput("table2")),
                             column(12, plotOutput("violins"))
                         )
                )
            )
        )
    )
)
