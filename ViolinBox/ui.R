##################################################
# Define UI for application that draws a heatmap #
##################################################
# navbarPage(title = "ViolinBox & Heatmaps",
#     tabPanel(title = "Data",
#              DTOutput(outputId = "table1"),
#              selectInput("Parameters",
#                         label = "Select Parameters to Plot",
#                         choices = as.character(paramNames),
#                         multiple = TRUE,
#                         selectize = FALSE)
#             ),
# navbarMenu(title = "Plots",
#     tabPanel(title = "Heatmaps",
#              plotOutput("heatmaps"),
#              actionButton("refreshPlot",
#                           label = "Refresh Plots"),
#              selectInput("hmPalette",
#                          label = "Select a color palette for heatmap",
#                          choices = hmPalette,
#                          multiple = FALSE,
#                          selectize = TRUE),
#              selectInput("catParam",
#                          label = "Select a Categorical Parameter",
#                          choices = c("None", as.character(paramNames)),
#                          selected = NULL)
#              ),
#     tabPanel(title = "Raincloud Plots",
#              plotOutput("violins"),
#              actionButton("refreshPlot",
#                           label = "Refresh Plots"),
#              checkboxInput("flipViolins",
#                            label = "Flip Violins",
#                            TRUE),
#              checkboxInput("darkTheme",
#                            label = "Dark Theme",
#                            FALSE),
#              checkboxInput("jitter",
#                            label = "Jitter",
#                            TRUE),
#              checkboxInput("violins",
#                            label = "Violins",
#                            TRUE),
#              checkboxInput("boxes",
#                            label = "Box-n-Whisker",
#                            FALSE),
#              checkboxInput("overlay",
#                            label = "Overlay Plots",
#                            FALSE),
#              checkboxInput("grpByCluster",
#                            label = "Group By Cluster",
#                            FALSE)
#              
#     )
#  )
# )

dashboardPage(skin = "blue",
              dashboardHeader(title = "ViolinBox & Heatmaps"),
              dashboardSidebar(
                sidebarMenu(
                  menuItem(text = "Data", tabName = "data", icon = icon("table")),
                  menuItem(text = "Heatmaps", tabName = "heatmaps", icon = icon("th")),
                  menuItem(text = "Raincloud Plots", tabName = "violins", icon = icon("soundcloud")),
                  menuItem(text = "Beeswarm Plots", tabName = "Beeswarms", icon = icon("bug")),
                  menuItem(text = "FlowSOM Clustering", tabName = "flowsom", icon = icon("sitemap"))
                )
                
              ),
              # ),
              dashboardBody(
                
                tags$head(tags$script('
      // Define function to set height of "map" and "map_container"
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();

        var boxHeight = window_height - header_height - 30;

        $("#Heatmaps").height(boxHeight);
        $("#heatmaps1").height(boxHeight - 20);
        $("#violins").height(boxHeight);
        $("#violins").height(boxHeight - 20);
        $("#Beeswarms").height(boxHeight);
        $("#Beeswarms").height(boxHeight - 20);
      };

      // Set input$box_height when the connection is established
      $(document).on("shiny:connected", function(event) {
        setHeight();
      });

      // Refresh the box height on every window resize event    
      $(window).on("resize", function(){
        setHeight();
      });
    ')),
                ##################################################
                # Define UI Layout for DATA TABLE Tab            #
                ##################################################
                tabItems(
                  tabItem(tabName = "data",
                          fluidRow(
                            box(title = "Select Data To Display",
                                width = 4,
                                status = "warning",
                                collapsible = T,
                                background = "light-blue",
                                selectInput("DTParameters",
                                            label = "Select Parameters to Plot",
                                            choices = as.character(paramNames),
                                            multiple = TRUE,
                                            selectize = TRUE)
                            ),
                            box(title ="Data Table",
                                width = 12,
                                status = "primary", solidHeader = T,
                                DTOutput("table1")
                            )
                            
                          )
                  ),
                  ##################################################
                  # Define UI Layout for HEATMAP Tab             #
                  ##################################################
                  tabItem(tabName = "heatmaps",
                          fluidRow(
                            box(title ="Heatmaps",
                                width = 12,
                                # height = 200,
                                status = "primary", solidHeader = T,
                                plotOutput("heatmaps1")
                            ),
                            box(title = "Choose Parameters to Plot",
                                width = 4,
                                status = "warning",
                                collapsible = T,
                                background = "light-blue",
                                selectInput("hmParameters",
                                            label = "Select Parameters to Plot",
                                            choices = as.character(paramNames),
                                            multiple = TRUE,
                                            selectize = TRUE)
                            ),
                            box(title = "Customize the Heatmap",
                                width = 4,
                                status = "warning",
                                collapsible = T,
                                background = "light-blue",
                                selectInput("hmPalette",
                                            label = "Select a color palette for heatmap",
                                            choices = hmPalette,
                                            multiple = FALSE,
                                            selectize = TRUE),
                                materialSwitch("scaleHM",
                                               label = "Normalize to Mode",
                                               right = TRUE,
                                               status = "warning",
                                               TRUE),
                                materialSwitch("dendrogram",
                                               label = "Add Dendrograms",
                                               right = TRUE,
                                               status = "warning",
                                               TRUE)
                            ),
                            box(title = "Select a Categorical Parameter",
                                width = 4,
                                collapsible = T,
                                status = "warning",
                                background = "light-blue",
                                selectInput("hmCatParam",
                                            label = "Select a Categorical Parameter",
                                            choices = c("None", as.character(paramNames)),
                                            selected = NULL),
                                actionBttn("HMrefreshPlot",
                                           label = "Refresh Plot",
                                           style = "material-flat",
                                           color = "success",
                                           icon = icon("sliders"))
                            )
                          )
                  ),
                  ##################################################
                  # Define UI Layout for RAINCLOUDS Tab             #
                  ##################################################
                  tabItem(tabName = "violins",
                          fluidRow(
                            box(title = "Make it Rain!",
                                width = 12,
                                status = "primary", solidHeader = T,
                                plotOutput("violins")
                            ),
                            box(title = "Choose Parameters to Plot",
                                width = 3,
                                height = "15%",
                                status = "warning",
                                collapsible = T,
                                background = "light-blue",
                                selectInput("Parameters",
                                            label = "Select Parameters to Plot",
                                            choices = as.character(paramNames),
                                            multiple = TRUE,
                                            selectize = TRUE),
                                actionBttn("refreshPlot",
                                           label = "Refresh Plot",
                                           style = "material-flat",
                                           color = "success",
                                           icon = icon("soundcloud"))
                            ),
                            box(title = "Customize your plot, then Refresh!",
                                width = 3,
                                height = "15%",
                                collapsible = T,
                                status = "warning",
                                background = "light-blue",
                                selectInput("vbPalette",
                                            label = "Select RainClouds Color Palette",
                                            choices = vbPalette,
                                            multiple = FALSE,
                                            selectize = TRUE),
                                materialSwitch("flipViolins",
                                               label = "Flip Violins",
                                               right = TRUE,
                                               status = "warning",
                                               TRUE),
                                materialSwitch("darkTheme",
                                               label = "Dark Theme",
                                               right = TRUE,
                                               status = "warning",
                                               FALSE),
                                materialSwitch("jitter",
                                               label = "Jitter",
                                               right = TRUE,
                                               status = "warning",
                                               TRUE),
                                materialSwitch("violins",
                                               label = "Violins",
                                               right = TRUE,
                                               status = "warning",
                                               TRUE),
                                materialSwitch("boxes",
                                               label = "Box-n-Whisker",
                                               right = TRUE,
                                               status = "warning",
                                               FALSE),
                                materialSwitch("addLegend",
                                               label = "Show Legend",
                                               right = TRUE,
                                               status = "warning",
                                               TRUE)
                            ),
                            box(title = "Select a Categorical Parameter",
                                width = 3,
                                height = "15%",
                                collapsible = T,
                                status = "warning",
                                background = "light-blue",
                                selectInput("catParam",
                                            label = "Select a Categorical Parameter",
                                            choices = c("None", as.character(paramNames)),
                                            selected = NULL),
                                materialSwitch("grpByParam",
                                               label = "Group By Parameter",
                                               right = TRUE,
                                               status = "warning",
                                               TRUE),
                                materialSwitch("statCompare",
                                               label = "Statistical Comparison",
                                               right = TRUE,
                                               status = "warning",
                                               FALSE),
                                materialSwitch("overlay",
                                               label = "Overlay Plots",
                                               right = TRUE,
                                               status = "warning",
                                               FALSE)
                                
                            ),
                            box(title = "Format the Plot",
                                width = 3,
                                height = "15%",
                                collapsible = T,
                                status = "warning",
                                background = "light-blue",
                                numericInput("bigfonts",
                                             label = "Label Font:",
                                             min = 1,
                                             max = 42,
                                             value = 18),
                                numericInput("smallfonts",
                                             label = "Axes Font:",
                                             min = 1,
                                             max = 42,
                                             value = 14)
                            )
                          )
                  ),
                  ##################################################
                  # Define UI Layout for BeeSwarm Tab              #
                  ##################################################
                  tabItem(tabName = "Beeswarms",
                          fluidRow(
                            box(title = "Beeswarm Plots",
                                width = 12,
                                status = "primary", solidHeader = T,
                                plotOutput("beeswarmPlot")
                            ),
                            box(title = "Choose Parameters to Plot",
                                width = 6,
                                height = "15%",
                                status = "warning",
                                collapsible = T,
                                background = "light-blue",
                                selectizeInput("bsParameters",
                                               label = "Select Parameters to Plot",
                                               choices = paramNames,
                                               selected = NULL,
                                               multiple = TRUE)
                            ),
                            box(title = "Select a Categorical Parameter",
                                width = 6,
                                height = "15%",
                                collapsible = T,
                                status = "warning",
                                background = "light-blue",
                                selectizeInput("bscatParam",
                                               label = "Select a Categorical Parameter",
                                               choices = c("None", as.character(paramNames)),
                                               selected = NULL,
                                               multiple = FALSE),
                                actionBttn("BSrefreshPlot",
                                           label = "Refresh Plot",
                                           style = "material-flat",
                                           color = "success",
                                           icon = icon("sliders")),
                                downloadButton("print", 
                                               label = "PRINT HTML",
                                               class = "Butt"),
                                tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))
                            )
                          )
                          
                  ),
                  
                  ##################################################
                  # Define UI Layout for FlowSOM Tab              #
                  ##################################################
                  tabItem(tabName = "flowsom",
                          fluidRow(
                            box(title = "Select Parameters in Cluster",
                                width = 4,
                                collapsible = T,
                                status = "warning",
                                background = "light-blue",
                                selectizeInput("flowsomParams",
                                               label = "Cluster on These Please",
                                               choices = fsomParams,
                                               selected = NULL,
                                               multiple = TRUE),
                                actionBttn("FSrefreshPlot",
                                           label = "Refresh Plot",
                                           style = "material-flat",
                                           color = "success",
                                           icon = icon("sitemap"))
                            ),
                            box(title = "Customize the Map",
                                width = 4,
                                collapsible = T,
                                status = "warning",
                                background = "light-blue",
                                selectizeInput("somPalette",
                                               label = "Select a color palette",
                                               choices = somPalette,
                                               selected = "reds",
                                               multiple = FALSE),
                                sliderInput("seedSlider",
                                            label = "Set Seed",
                                            value = 123,
                                            min = 3,
                                            max = 262421,
                                            step = 1)
                                
                            ),
                            box(title = "FlowSOM",
                                width = 12,
                                status = "primary", solidHeader = T,
                                plotOutput("flowsomPlot")
                            )
                            
                          )
                  )
                  # tabItems(
                  #   tabItem(tabName = "data",
                  #           DTOutput(outputId = "table1")
                  #           ),
                  #   tabItem(tabName = "heatmaps",
                  #          plotOutput("heatmaps")
                  #          ),
                  #   tabItem(tabName = "violins",
                  #           plotOutput("violins")
                  #           )
                  # )
                )
              ))




# fluidPage(
#
#     # Application title
#     titlePanel("ViolinBox Plugin Proto"),
#
#     # Sidebar with a select input for the parameters to heatmap
#     sidebarLayout(
#         sidebarPanel(
#             # fileInput("file",
#             #           h3("Upload a CSV File:"),
#             #              multiple = FALSE,
#             #              accept = c(".csv")),
#             selectInput("Parameters",
#                         label = "Select Parameters to Heatmap",
#                         choices = as.character(paramNames),
#                         multiple = TRUE,
#                         selectize = TRUE),
#             selectInput("catParam",
#                         label = "Select a Categorical Parameter",
#                         choices = c("None", as.character(paramNames)),
#                         selected = NULL),
#             checkboxInput("flipViolins",
#                           label = "Flip Violins",
#                           TRUE),
#             checkboxInput("darkTheme",
#                           label = "Dark Theme",
#                           FALSE),
#             checkboxInput("jitter",
#                           label = "Jitter",
#                           TRUE),
#             checkboxInput("violins",
#                           label = "Violins",
#                           TRUE),
#             checkboxInput("boxes",
#                           label = "Box-n-Whisker",
#                           FALSE),
#             # checkboxInput("grpClust",
#             #               label = "Group By Cluster",
#             #               FALSE),
#             # checkboxGroupInput("violinCheckGroup",
#             #                    label = h3("Violin Plot Options"),
#             #                    choices = list("Group by Cluster" = 1,
#             #                                   "Flip Violins" = "parFlipGraph",
#             #                                   "Dark Theme" = "parDarkTheme",
#             #                                   "Add Violins" = 4,
#             #                                   "Add Jitter" = 5,
#             #                                   "Add Boxes" = 6
#             #                                   ),
#             #                    selected = TRUE),
#             actionButton("refreshPlot",
#                          label = "Refresh Plots")
#         ),
#
#     # Show a plot of the generated table, heatmap, and raincloud plots.
#     mainPanel(
#         tabsetPanel(
#             tabPanel("Heatmap",
#                      fluidRow(
#                         column(12, DTOutput("table1")),
#                         column(12, plotOutput("heatmaps"))
#                      )),
#             tabPanel("Violin Plots",
#                      fluidRow(
#                          column(12, DTOutput("table2")),
#                          column(12, plotOutput("violins"))
#                      )
#             )
#         )
#     )
# )

