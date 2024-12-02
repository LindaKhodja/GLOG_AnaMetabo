library(shiny)
library(visNetwork)
library(DT)

ui <- fluidPage(
  titlePanel("Reactome SBML Graph Visualization"),
  sidebarLayout(
    sidebarPanel(
      fileInput("sbml_file", "Upload an SBML File (.xml)", accept = c(".xml", ".sbml")),
      actionButton("generate_graph", "Generate Graph"),
      tags$hr(),
      h4("Modify Graph"),
      textInput("add_node_label", "New Node Label"),
      textInput("add_node_group", "New Node Group"),
      actionButton("add_node", "Add Node"),
      actionButton("delete_node", "Delete Selected Node"),
      tags$hr(),
      h4("Add Edge"),
      selectInput("edge_from", "From Node (Label)", choices = NULL),
      selectInput("edge_to", "To Node (Label)", choices = NULL),
      textInput("edge_label", "Edge Label"),
      actionButton("add_edge", "Add Edge"),
      tags$hr(),
      h4("Delete Edge"),
      selectInput("delete_edge_from", "From Node (Label)", choices = NULL),
      selectInput("delete_edge_to", "To Node (Label)", choices = NULL),
      actionButton("delete_edge", "Delete Edge"),
      tags$hr(),
      h4("Save Graph"),
      textInput("save_filename", "File Name", value = "modified_graph.xml"),
      downloadButton("download_sbml", "Download SBML File"),
      tags$hr(),
      h4("Graph Summary"),
      textOutput("graph_summary"),
      tags$hr(),
      h4("Cellular Compartments"),
      uiOutput("legend")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Graph Visualization", visNetworkOutput("network", height = "700px")),
        tabPanel("Nodes and Edges", h4("Nodes Table"), DTOutput("nodes_table"), h4("Edges Table"), DTOutput("edges_table"))
      )
    )
  )
)
