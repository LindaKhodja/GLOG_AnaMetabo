# ui.R
# User Interface (UI) for the AnaMetabo application
# This UI is built using the Shiny Dashboard layout to organize the application structure.

ui <- dashboardPage(
  skin = "purple",  # Choose the theme for the application
  
  # Header of the Dashboard (title and logo can be customized)
  dashboardHeader(
    title = tags$div(
      tags$img(src = "logo.png", height = "40px", style = "margin-right: 10px;"),  # Add logo in the header
      "AnaMetabo™"
    )
  ),
  
  # Sidebar with navigation menu to switch between tabs
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Network Visualization", tabName = "network_vis", icon = icon("project-diagram")),
      menuItem("Advanced Analysis", tabName = "advanced_analysis", icon = icon("chart-line")),
      menuItem("Tutorial & Documentation", tabName = "tutorial", icon = icon("book"))
    )
  ),
  
  # Body of the Dashboard where the main content is displayed
  dashboardBody(
    tabItems(
      
      # --- Home Tab ---
      tabItem(tabName = "home",
              h2("Welcome to AnaMetabo!"),  # Title of the home page
              tags$img(src = "logo.png", height = "250px", style = "float: right; margin-right: 30px;"),  # Add logo in the Home Tab
              p("AnaMetabo is a bioinformatics tool designed for the analysis and visualization of metabolic networks from SBML files."),
              
              h3("Features by Tab:"),
              h4("1. Network Visualization"),
              tags$ul(
                tags$li("Interactively explore networks"),
                tags$li("Customize the graph (add/remove nodes and edges)"),
                tags$li("Choose from multiple graph layout types"),
                tags$li("Download data tables")
              ),
              
              h4("2. Advanced Analysis"),
              tags$ul(
                tags$li("Centrality Analysis: Degree, Closeness, and Betweenness calculations"),
                tags$li("Automatic Clustering: Identify related groups"),
                tags$li("Shortest Path Search: Find shortest paths between nodes"),
                tags$li("Community Metrics and Network Resilience calculations")
              ),
              
              h4("Demo File:"),
              p("You can download a sample SBML file to test the available features."),
              p("Source : https://reactome.org/content/detail/R-HSA-168255"),
              downloadButton("download_demo_sbml", "Download SBML File"),
              
              tags$hr(),
              
              h3("About the Project"),
              p("This webserver was developed as part of a Software Engineering project for the Master's program in Bioinformatics (M2) at the University of Bordeaux."),
              p(
                tags$small("Please note that AnaMetabo™ is a fictitious brand name used for educational purposes only.")
              ),
              
              tags$footer(
                p("Created by Céline Hosteins and Team."),
                style = "position: fixed; bottom: 0; width: 100%; text-align: center; 
                  background-color: #f8f9fa; padding: 10px; font-size: 12px; color: #555;"
              )
      ),
      
      # --- Network Visualization Tab ---
      tabItem(tabName = "network_vis",
              fluidPage(
                titlePanel("SBML Network Visualization"),  # Title for the Network Visualization page
                
                sidebarLayout(
                  sidebarPanel(
                    # Sidebar content with inputs for file selection and graph options
                    fileInput("sbml_file", "Import an SBML File (.xml)", accept = c(".xml", ".sbml")),  # Input for SBML file upload
                    actionButton("generate_graph", "Generate Graph"),  # Button to trigger graph generation
                    tags$hr(),
                    
                    # Dropdown for selecting the layout type for network visualization
                    selectInput("layout_choice", "Select a Layout:", 
                                choices = c(
                                  "Force Atlas 2 Based" = "forceAtlas2Based", 
                                  "Barnes Hut" = "barnesHut", 
                                  "Hierarchical" = "hierarchical", 
                                  "Circular" = "circular"
                                ),
                                selected = "forceAtlas2Based"  # Default layout option
                    ),
                    tags$hr(),
                    
                    # New buttons for saving graph and downloading data
                    downloadButton("download_nodes_csv", "Download Nodes as CSV"),
                    downloadButton("download_edges_csv", "Download Edges as CSV"),
                    tags$hr(),
                    
                    # Placeholder for dynamic UI elements for legend or node details
                    uiOutput("sidebar_legend"),
                    width = 3  # Sidebar width
                  ),
                  
                  # Main panel with the content displayed based on selected tab
                  mainPanel(
                    tabsetPanel(
                      # Tab for visualizing the network graph
                      tabPanel("Graph Visualization",
                               div(style = "position: relative;",
                                   downloadButton("download_sbml", label = NULL, 
                                                  icon = icon("download"), 
                                                  style = "position: absolute; top: 10px; right: 10px; z-index: 1000;"),
                                   div(style = "text-align: center; font-size: 16px; margin-top: 10px;", 
                                       textOutput("graph_summary")),  # Graph summary information
                                   visNetworkOutput("network", height = "580px")  # Output for visualizing the network graph
                               )
                      ),
                      
                      # Tab for displaying nodes and edges in tables
                      tabPanel("Nodes and Edges",
                               h4("Nodes Table"),
                               DTOutput("nodes_table"),  # Display nodes table using DataTables
                               h4("Edges Table"),
                               DTOutput("edges_table")  # Display edges table using DataTables
                      )
                    ),
                    width = 9  # Main panel width
                  )
                ),
                
                # Footer with controls for graph modification
                tags$footer(
                  style = "background-color: #f8f9fa; padding: 15px; border-top: 1px solid #dee2e6; text-align: center;",
                  h4("Graph Modification"),
                  div(
                    actionButton("add_node", "Add Node", style = "margin-right: 10px;"),
                    actionButton("delete_node", "Delete Selected Node", style = "margin-right: 10px;"),
                    actionButton("add_edge_button", "Add Edge", style = "margin-right: 10px;"),
                    actionButton("delete_edge", "Delete Edge"),
                    style = "margin-top: 10px;"
                  )
                )
              )
      ),
      
      # --- Advanced Analysis Tab ---
      tabItem(tabName = "advanced_analysis",
              fluidPage(
                titlePanel("Advanced Network Analysis"),  # Title for the Advanced Analysis page
                
                sidebarLayout(
                  sidebarPanel(
                    # Sidebar content for advanced analysis inputs
                    fileInput("sbml_file_advanced", "Import an SBML File (.xml)", accept = c(".xml", ".sbml")),  # Input for uploading SBML file
                    actionButton("load_graph", "Load Graph"),  # Button to load graph for analysis
                    tags$hr(),
                    
                    # Dropdown for selecting the analysis type
                    selectInput("menu", "Select Analysis Type:",
                                choices = list(
                                  "Graph Visualization" = "visualization",
                                  "Centrality Analysis" = "centrality",
                                  "Cluster Analysis" = "clusters",
                                  "Shortest Path" = "shortest_path",
                                  "Community Metrics" = "community_metrics",
                                  "Network Resilience" = "resilience"
                                )),
                    
                    # Conditional panels for different types of analysis (shown based on user selection)
                    conditionalPanel(
                      condition = "input.menu == 'centrality'",
                      selectInput("centrality_metric", "Select Centrality Metric:",
                                  choices = c("Degree" = "degree", "Closeness" = "closeness", "Betweenness" = "betweenness")),
                      actionButton("analyze_centrality", "Analyze Centrality")  # Button to analyze centrality
                    ),
                    
                    conditionalPanel(
                      condition = "input.menu == 'shortest_path'",
                      selectInput("from_node", "Start Node:", choices = NULL),
                      selectInput("to_node", "End Node:", choices = NULL),
                      actionButton("find_shortest_path", "Find Shortest Path")  # Button to find the shortest path
                    ),
                    
                    conditionalPanel(
                      condition = "input.menu == 'clusters'",
                      actionButton("analyze_clusters", "Identify Clusters")  # Button to perform clustering analysis
                    ),
                    
                    conditionalPanel(
                      condition = "input.menu == 'community_metrics'",
                      actionButton("analyze_community_metrics", "Analyze Community Metrics")  # Button to analyze community metrics
                    ),
                    
                    conditionalPanel(
                      condition = "input.menu == 'resilience'",
                      actionButton("analyze_resilience", "Analyze Network Resilience")  # Button to analyze network resilience
                    ),
                    
                    tags$hr(),
                    textOutput("graph_summary_advanced")  # Display analysis summary
                  ),
                  
                  # Main panel for displaying the results of advanced analysis
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Graph", visNetworkOutput("advanced_network", height = "700px")),  # Graph output
                      tabPanel("Results", DTOutput("analysis_results"))  # Display analysis results in a table
                    )
                  )
                )
              )
      ),
      # --- Tutorial & Documentation Tab ---
      tabItem(tabName = "tutorial",
              fluidPage(
                titlePanel("Tutorial and Documentation"),  # Title
                
                fluidRow(
                  column(12,
                         h3("Video Tutorial"),
                         p("Watch the video tutorial below to learn how to use the platform:"),
                         
                         # Embed Local Video
                         tags$video(width = "80%", controls = TRUE,
                                    tags$source(src = "tutorial_video.mp4", type = "video/mp4")),
                         
                         h3("Documentation"),
                         p("For more in-depth documentation, please refer to the link below:"),
                         tags$ul(
                           tags$li(tags$a(href = "user_guide.html", "Full User Guide"))
                         )
                         
                  )
                )
              )
      )
    )
  )
)
