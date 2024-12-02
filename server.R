source("utils.R")

server <- function(input, output, session) {
  # Reactive storage
  graph_data <- reactiveValues(
    nodes = data.frame(id = character(), label = character(), group = character(), stringsAsFactors = FALSE),
    edges = data.frame(from = character(), to = character(), label = character(), stringsAsFactors = FALSE)
  )
  
  # Event Observers and Reactive Logic
  observeEvent(input$generate_graph, {
    req(input$sbml_file)
    file_path <- input$sbml_file$datapath
    data <- load_sbml_data(file_path)
    graph_data$nodes <- data$nodes
    graph_data$edges <- data$edges
    
    # Update UI dropdowns
    update_node_selectors(session, graph_data$nodes)
  })
  
  # Other observer events for add/remove nodes and edges, graph summary, and file saving go here...
  
  # Graph rendering
  output$network <- renderVisNetwork({
    req(graph_data$nodes, graph_data$edges)
    render_graph(graph_data$nodes, graph_data$edges)
  })
}
