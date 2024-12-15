#server.R
source("utils.R")

# Serveur (backend principal)
server <- function(input, output, session) {
  # Réactif pour stocker les données du graphe
  # SBML Demo File Download Button
  output$download_demo_sbml <- downloadHandler(
    filename = function() {
      "H5N1_infection.sbml"
    },
    content = function(file) {
      # Ensure the file example_file.sbml exists in the data/ folder
      file.copy("examples/Influenza(H5N1)_infection.sbml", file)
    }
  )
  
  graph_data <- reactiveValues(
    nodes = data.frame(id = character(), label = character(), color = character(), compartment = character(), stringsAsFactors = FALSE),
    edges = data.frame(from = character(), to = character(), label = character(), stringsAsFactors = FALSE)
  )
  
  # Reactive Values pour l'onglet analysis
  graph_data_analysis <- reactiveValues(
    nodes = data.frame(),
    edges = data.frame(),
    igraph = NULL,
    analysis_results = data.frame()
  )
  
  # Réactif pour suivre si le graphe est généré
  graph_generated <- reactiveVal(FALSE)
  
  load_sbml_data <- memoise(load_sbml_data)
  
  # Gestion du clic sur le bouton "Add Node"
  observeEvent(input$add_node, {
    showModal(modalDialog(
      title = "Add a node",
      textInput("node_id", "Node ID:", value = ""),
      textInput("node_label", "Node name:", value = ""),
      selectInput("node_shape", "Node shape:", 
                  choices = c("circle", "diamond", "square"), 
                  selected = "circle"),
      conditionalPanel(
        condition = "input.node_shape != 'square'",  # Montrer ce champ uniquement si la forme n'est pas "square"
        selectInput("node_compartment", "Compartiment :", 
                    choices = c(names(compartment_colors)), 
                    selected = "cytosol")
      ),
      footer = tagList(
        actionButton("confirm_add_node", "Confirm"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$confirm_add_node, {
    req(input$node_id, input$node_label, input$node_shape)
    
    # Vérifier si l'ID existe déjà
    if (input$node_id %in% graph_data$nodes$id) {
      showNotification("ID already exists!", type = "error")
      return()
    }
    
    # Définir les propriétés spécifiques pour les nœuds
    if (input$node_shape == "square") {
      # Forcer les propriétés pour les carrés (réactions)
      node_compartment <- "reaction"
      node_size <- 20
      node_color_background <- "red"
      border_color <- "black"
    } else {
      # Propriétés par défaut pour les autres formes
      node_compartment <- input$node_compartment
      node_size <- 50
      node_color_background <- "lightgrey"
      border_color <- if (node_compartment %in% names(compartment_colors)) {
        compartment_colors[[node_compartment]]
      } else {
        "black"  # Couleur par défaut si le compartiment n'est pas trouvé
      }
    }
    
    # Créer le nœud avec toutes les propriétés nécessaires
    new_node <- data.frame(
      id = input$node_id,
      label = input$node_label,
      shape = input$node_shape,
      size = node_size,
      font.size = 20,
      color.background = node_color_background,
      color.border = border_color,
      compartment = node_compartment,
      stringsAsFactors = FALSE
    )
    
    # Synchronisation des colonnes entre les nœuds existants et le nouveau
    all_columns <- union(names(graph_data$nodes), names(new_node))
    for (col in setdiff(all_columns, names(graph_data$nodes))) {
      graph_data$nodes[[col]] <- NA
    }
    for (col in setdiff(all_columns, names(new_node))) {
      new_node[[col]] <- NA
    }
    
    # Ajouter le nœud aux données existantes
    graph_data$nodes <- rbind(graph_data$nodes, new_node)
    
    # Appliquer la fonction de calcul des tailles
    graph_data$nodes <- calculate_node_size(graph_data$nodes)
    
    # Mettre à jour les sélecteurs
    updateSelectInput(session, "edge_from", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    updateSelectInput(session, "edge_to", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    updateSelectInput(session, "delete_edge_from", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    updateSelectInput(session, "delete_edge_to", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    
    # Notification de succès
    showNotification("Node successfully added!", type = "message")
    removeModal()
  })
  
  observeEvent(input$delete_node, {
    req(input$network_selected)  # Vérifie qu'un nœud est sélectionné dans le graphe
    
    # Récupérer l'ID du nœud sélectionné
    selected_node <- input$network_selected
    
    # Supprimer le nœud sélectionné et ses arêtes associées
    graph_data$nodes <- graph_data$nodes[graph_data$nodes$id != selected_node, ]
    graph_data$edges <- graph_data$edges[!(graph_data$edges$from == selected_node | graph_data$edges$to == selected_node), ]
    
    # Recalculer les tailles après suppression
    graph_data$nodes <- calculate_node_size(graph_data$nodes)
    
    # Mettre à jour les sélecteurs
    updateSelectInput(session, "edge_from", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    updateSelectInput(session, "edge_to", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    updateSelectInput(session, "delete_edge_from", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    updateSelectInput(session, "delete_edge_to", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    
    # Notification de succès
    showNotification("Node successfully deleted!", type = "message")
  })
  
  # Modale pour ajouter une arête
  observeEvent(input$add_edge_button, {
    showModal(modalDialog(
      title = "Add Edge",
      selectInput("edge_from", "From Node:", 
                  choices = setNames(graph_data$nodes$id, graph_data$nodes$label)),
      selectInput("edge_to", "To Node:", 
                  choices = setNames(graph_data$nodes$id, graph_data$nodes$label)),
      textInput("edge_label", "Edge Label (Optional):"),
      selectInput("arrow_type", "Arrow Type:",
                  choices = list("Basic (Black)" = "basic",
                                 "Inhibition (Red)" = "inhibition",
                                 "Activation (Dashed Green)" = "activation")),
      footer = tagList(
        actionButton("confirm_add_edge", "Confirm"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$confirm_add_edge, {
    req(input$edge_from, input$edge_to, input$arrow_type)
    
    # Check if nodes exist
    if (!(input$edge_from %in% graph_data$nodes$id) || !(input$edge_to %in% graph_data$nodes$id)) {
      showNotification("Specified nodes do not exist.", type = "error")
      return()
    }
    
    # Determine edge properties based on arrow type
    edge_color <- "black"
    edge_dashes <- FALSE
    
    if (input$arrow_type == "inhibition") {
      edge_color <- "red"
    } else if (input$arrow_type == "activation") {
      edge_color <- "green"
      edge_dashes <- TRUE
    }
    
    # Create the new edge
    new_edge <- data.frame(
      from = input$edge_from,
      to = input$edge_to,
      arrows = "to", # Arrow points to the target node
      color = edge_color, # Set color based on selection
      dashes = edge_dashes, # Dashed line for activation
      width = 1, # Default edge width
      label = input$edge_label, # Optional label
      stringsAsFactors = FALSE
    )
    
    # Synchronize columns with existing edges
    all_columns <- union(names(graph_data$edges), names(new_edge))
    for (col in setdiff(all_columns, names(graph_data$edges))) {
      graph_data$edges[[col]] <- NA
    }
    for (col in setdiff(all_columns, names(new_edge))) {
      new_edge[[col]] <- NA
    }
    
    # Add the new edge to the data
    graph_data$edges <- rbind(graph_data$edges, new_edge)
    
    # Success notification
    showNotification("Edge successfully added!", type = "message")
    removeModal()
  })
  
  
  observeEvent(input$delete_edge, {
    showModal(modalDialog(
      title = "Delete Edge",
      # From Node with search feature
      selectizeInput("delete_edge_from", "From Node:", 
                     choices = setNames(graph_data$nodes$id, graph_data$nodes$label),
                     selected = NULL, 
                     options = list(placeholder = "Select or search a node")),
      # To Node with search feature
      selectizeInput("delete_edge_to", "To Node:", 
                     choices = setNames(graph_data$nodes$id, graph_data$nodes$label),
                     selected = NULL, 
                     options = list(placeholder = "Select or search a node")),
      # Footer with Confirm and Cancel buttons
      footer = tagList(
        modalButton("Cancel"), # Cancel button
        actionButton("confirm_delete_edge", "Delete") # Confirmation button
      )
    ))
  })
  
  observeEvent(input$confirm_delete_edge, {
    req(input$delete_edge_from, input$delete_edge_to) # Ensure required inputs are provided
    
    # Check if any matching edges exist
    edges_to_delete <- graph_data$edges[
      graph_data$edges$from == input$delete_edge_from &
        graph_data$edges$to == input$delete_edge_to, 
    ]
    
    if (nrow(edges_to_delete) == 0) {
      showNotification("No matching edge found to delete.", type = "error")
      return()
    }
    
    # Remove the matching edge(s)
    graph_data$edges <- graph_data$edges[!(
      graph_data$edges$from == input$delete_edge_from &
        graph_data$edges$to == input$delete_edge_to
    ), ]
    
    # Notify the user of successful deletion
    showNotification("Edge successfully deleted!", type = "message")
    
    # Close the modal
    removeModal()
  })
  
  # --- Code for downloading nodes CSV ---
  output$download_nodes_csv <- downloadHandler(
    filename = function() { "nodes.csv" },
    content = function(file) {
      req(graph_data$nodes)  # Ensure nodes are available
      write.csv(graph_data$nodes, file, row.names = FALSE)
      # Notify the user
      showNotification("Nodes savec in CSV", type = "message")
    }
  )
  
  # --- Code for downloading edges CSV ---
  output$download_edges_csv <- downloadHandler(
    filename = function() { "edges.csv" },
    content = function(file) {
      req(graph_data$edges)  # Ensure edges are available
      write.csv(graph_data$edges, file, row.names = FALSE)
      showNotification("Edges savec in CSV", type = "message")
    }
  )
  
  # Charger un fichier SBML
  observeEvent(input$generate_graph, {
    req(input$sbml_file)
    showNotification("Loading graph... please wait.", type = "message")
    
    sbml_data <- load_sbml_data(input$sbml_file$datapath)
    graph_data$nodes <- sbml_data$nodes
    graph_data$edges <- sbml_data$edges
    
    # Recalculer les tailles après génération
    graph_data$nodes <- calculate_node_size(graph_data$nodes)
    
    # Mettre à jour les sélecteurs
    updateSelectInput(session, "edge_from", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    updateSelectInput(session, "edge_to", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    updateSelectInput(session, "delete_edge_from", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
    updateSelectInput(session, "delete_edge_to", choices = setNames(graph_data$nodes$id, graph_data$nodes$label))
  })
  
  
  
  
  ###### analysis ###########################################################################################
  #server_analysis
  
  # Load Graph
  observeEvent(input$load_graph, {
    req(input$sbml_file_advanced)
    showNotification("Loading graph... please wait.", type = "message")
    
    # Parse SBML file
    sbml_data_2 <- load_sbml_data(input$sbml_file_advanced$datapath)
    graph_data_analysis$nodes <- sbml_data_2$nodes
    graph_data_analysis$nodes$color <- "lightgray"
    graph_data_analysis$nodes$value <- 1
    
    graph_data_analysis$edges <- sbml_data_2$edges
    graph_data_analysis$edges$color <- "black"
    
    # Convert to igraph object
    graph_data_analysis$igraph <- graph_from_data_frame(
      d = graph_data_analysis$edges, 
      vertices = graph_data_analysis$nodes, 
      directed = TRUE
    )
    
    output$graph_summary_advanced <- renderText({
      paste("Number of nodes:", nrow(graph_data_analysis$nodes), 
            "| Number of edges:", nrow(graph_data_analysis$edges))
    })
    
    # Update node selection for shortest path
    updateSelectInput(session, "from_node", 
                      choices = setNames(graph_data_analysis$nodes$id, graph_data_analysis$nodes$label))
    updateSelectInput(session, "to_node", 
                      choices = setNames(graph_data_analysis$nodes$id, graph_data_analysis$nodes$label))
    
    # Render initial graph
    output$advanced_network <- renderVisNetwork({
      visNetwork(graph_data_analysis$nodes, graph_data_analysis$edges) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visEdges(arrows = "to") %>%
        visInteraction(zoomView = TRUE, dragView = TRUE)
    })
    showNotification("Graph successfully loaded!", type = "message")
  })
  
  # Centrality Analysis
  observeEvent(input$analyze_centrality, {
    req(graph_data_analysis$igraph)
    showNotification("Analyzing centrality... please wait.", type = "message")
    
    # Réinitialiser la légende
    output$sidebar_legend <- renderUI({
      tags$div("No legend to display")
    })
    
    metric <- switch(input$centrality_metric,
                     "degree" = degree(graph_data_analysis$igraph, mode = "all"),
                     "closeness" = closeness(graph_data_analysis$igraph, mode = "all"),
                     "betweenness" = betweenness(graph_data_analysis$igraph, directed = TRUE))
    
    graph_data_analysis$nodes$value <- metric
    graph_data_analysis$nodes$color <- colorRampPalette(c("lightblue", "darkblue"))(100)[
      as.numeric(cut(metric, breaks = 100))]
    
    graph_data_analysis$analysis_results <- data.frame(
      ID = graph_data_analysis$nodes$id,
      Label = graph_data_analysis$nodes$label,
      Centrality = metric
    )
    
    # Update Results Table
    output$analysis_results <- renderDT({
      datatable(graph_data_analysis$analysis_results, options = list(pageLength = 10))
    })
    
    # Update Graph
    output$advanced_network <- renderVisNetwork({
      visNetwork(graph_data_analysis$nodes, graph_data_analysis$edges) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visEdges(arrows = "to") %>%
        visInteraction(zoomView = TRUE, dragView = TRUE)
    })
  })
  
  # Cluster Analysis
  observeEvent(input$analyze_clusters, {
    req(graph_data_analysis$igraph)
    showNotification("Identifying clusters... please wait.", type = "message")
    
    # Réinitialiser la légende
    output$sidebar_legend <- renderUI({
      tags$div("No legend to display")
    })
    
    clusters <- cluster_walktrap(graph_data_analysis$igraph)
    membership <- membership(clusters)
    graph_data_analysis$nodes$color <- colorRampPalette(c("lightgreen", "darkgreen"))(length(unique(membership)))[membership]
    
    graph_data_analysis$analysis_results <- data.frame(
      ID = graph_data_analysis$nodes$id,
      Label = graph_data_analysis$nodes$label,
      Cluster = membership
    )
    
    # Update Results Table
    output$analysis_results <- renderDT({
      datatable(graph_data_analysis$analysis_results, options = list(pageLength = 10))
    })
    
    # Update Graph
    output$advanced_network <- renderVisNetwork({
      visNetwork(graph_data_analysis$nodes, graph_data_analysis$edges) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visEdges(arrows = "to") %>%
        visInteraction(zoomView = TRUE, dragView = TRUE)
    })
  })
  
  # Shortest Path
  observeEvent(input$find_shortest_path, {
    req(graph_data_analysis$igraph, input$from_node, input$to_node)
    showNotification("Finding shortest path... please wait.", type = "message")
    
    # Réinitialiser la légende
    output$sidebar_legend <- renderUI({
      tags$div("No legend to display")
    })
    
    # Vérification de la validité du graphe
    if (!igraph::is_igraph(graph_data_analysis$igraph)) {
      showNotification("Invalid graph structure!", type = "error")
      return()
    }
    
    path <- calculate_shortest_path(graph_data_analysis, input$from_node, input$to_node)
    
    if (is.null(path) || length(path$vpath[[1]]) == 0) {
      showNotification("No path found!", type = "error")
      return()
    }
    
    # Réinitialiser les couleurs
    graph_data_analysis$nodes$color <- "lightgray"
    graph_data_analysis$edges$color <- "black"
    
    # Mettre à jour les couleurs des nœuds
    node_ids <- igraph::V(graph_data_analysis$igraph)[path$vpath[[1]]]$name
    edge_ids <- igraph::E(graph_data_analysis$igraph)[path$epath[[1]]]$name
    
    graph_data_analysis$nodes$color[graph_data_analysis$nodes$id %in% node_ids] <- "green"
    graph_data_analysis$edges$color[graph_data_analysis$edges$id %in% edge_ids] <- "green"
    
    # Afficher les résultats dans la table
    graph_data_analysis$analysis_results <- data.frame(
      Path_Nodes = node_ids
    )
    
    # Mettre à jour la table des résultats
    output$analysis_results <- renderDT({
      datatable(graph_data_analysis$analysis_results, options = list(pageLength = 10))
    })
    
    # Mettre à jour le graphe
    output$advanced_network <- renderVisNetwork({
      visNetwork(graph_data_analysis$nodes, graph_data_analysis$edges) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visEdges(arrows = "to") %>%
        visInteraction(zoomView = TRUE, dragView = TRUE)
    })
  })
  
  # Community metrics
  observeEvent(input$analyze_community_metrics, {
    req(graph_data_analysis$igraph)
    
    showNotification("Calculating community metrics... please wait.", type = "message")
    
    metrics <- tryCatch({
      calculate_community_metrics(graph_data_analysis)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
    
    if (is.null(metrics) || nrow(metrics) == 0) {
      showNotification("No community metrics found!", type = "error")
      return()
    }
    
    # Mise à jour des résultats
    graph_data_analysis$community_results <- metrics
    
    # Coloration par communauté
    community_colors <- rainbow(length(unique(metrics$Community)))
    graph_data_analysis$nodes$color <- community_colors[igraph::membership(
      igraph::cluster_louvain(igraph::as.undirected(graph_data_analysis$igraph, mode = "collapse"))
    )]
    graph_data_analysis$community_colors <- community_colors
    
    # Mise à jour de la légende DANS observeEvent
    output$sidebar_legend <- renderUI({
      req(graph_data_analysis$community_results, graph_data_analysis$community_colors)
      
      unique_communities <- unique(graph_data_analysis$community_results$Community)
      community_colors <- graph_data_analysis$community_colors
      
      legend_items <- lapply(seq_along(unique_communities), function(i) {
        tags$div(
          style = paste("margin-bottom: 5px; font-size: 14px; color:", community_colors[i], ";"),
          tags$strong(paste("Community", unique_communities[i])),
          tags$span(style = paste("background-color:", community_colors[i], 
                                  "; display: inline-block; width: 20px; height: 20px; margin-left: 10px;"))
        )
      })
      do.call(tagList, legend_items)
    })
    
    # Mise à jour du graphe
    output$advanced_network <- renderVisNetwork({
      visNetwork(graph_data_analysis$nodes, graph_data_analysis$edges) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visEdges(arrows = "to") %>%
        visInteraction(zoomView = TRUE, dragView = TRUE)
    })
  })
  
  #Network resilience
  observeEvent(input$analyze_resilience, {
    req(graph_data_analysis$igraph)
    showNotification("Analyzing network resilience... please wait.", type = "message")
    
    # Réinitialiser la légende
    output$sidebar_legend <- renderUI({
      tags$div("No legend to display")
    })
    
    # Calcul de la résilience
    resilience_results <- tryCatch({
      calculate_network_resilience(graph_data_analysis)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
    
    if (is.null(resilience_results)) {
      showNotification("No resilience data found!", type = "error")
      return()
    }
    
    # Mise à jour des résultats dans la table
    graph_data_analysis$resilience_results <- resilience_results
    output$analysis_results <- renderDT({
      datatable(resilience_results, options = list(pageLength = 10))
    })
    
    # Application du gradient de couleur
    min_size <- min(resilience_results$Largest_Component_Size)
    max_size <- max(resilience_results$Largest_Component_Size)
    color_palette <- colorRampPalette(c("darkred", "lightyellow"))  # Gradient de couleurs
    
    # Calcul des couleurs pour chaque nœud supprimé
    node_colors <- sapply(resilience_results$Largest_Component_Size, function(size) {
      color_palette(100)[as.numeric(cut(size, breaks = 100))]
    })
    
    # Mise à jour des couleurs des nœuds
    graph_data_analysis$nodes$color <- "lightgray"  # Réinitialiser toutes les couleurs
    for (i in seq_along(resilience_results$Removed_Node)) {
      graph_data_analysis$nodes$color[graph_data_analysis$nodes$id == resilience_results$Removed_Node[i]] <- node_colors[i]
    }
    
    # Mise à jour du graphe
    output$advanced_network <- renderVisNetwork({
      visNetwork(graph_data_analysis$nodes, graph_data_analysis$edges) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visEdges(arrows = "to") %>%
        visInteraction(zoomView = TRUE, dragView = TRUE)
    })
    
    showNotification("Resilience analysis complete!", type = "message")
  })
  
  
  # Légende dynamique dans la sidebar
  output$sidebar_legend <- renderUI({
    if (nrow(graph_data$nodes) > 0) {
      # Obtenir les compartiments uniques avec leurs couleurs
      unique_compartments <- unique(graph_data$nodes[, c("compartment", "color.border"), drop = FALSE])
      unique_compartments <- unique_compartments[complete.cases(unique_compartments), ]
      
      div(
        h4("Legend"),
        tags$ul(
          lapply(1:nrow(unique_compartments), function(i) {
            tags$li(
              style = "list-style-type: none; margin-bottom: 5px; display: flex; align-items: center;",
              div(
                style = paste0(
                  "width: 20px; height: 20px; border: 1px solid ", unique_compartments$color.border[i], 
                  "; background-color: transparent; border-radius: 50%; margin-right: 10px;"  # Bordures arrondies pour un rond
                )
              ),
              span(unique_compartments$compartment[i], style = "color: black; font-size: 14px;")
            )
          })
        )
      )
    } else {
      div(h4("No legend to display"))
    }
  })
  
  # Télécharger le fichier SBML
  output$download_sbml <- downloadHandler(
    filename = function() {
      input$save_filename
    },
    content = function(file) {
      save_sbml(graph_data$nodes, graph_data$edges, file)
    }
  )
  
  # Résumé du graphe
  output$graph_summary <- renderText({
    num_nodes <- nrow(graph_data$nodes)
    num_edges <- nrow(graph_data$edges)
    paste("Nodes:", num_nodes, "| Edges:", num_edges)
  })
  
  # Tableaux interactifs
  output$nodes_table <- renderDT({
    datatable(graph_data$nodes, options = list(pageLength = 10))
  })
  
  output$edges_table <- renderDT({
    datatable(graph_data$edges, options = list(pageLength = 10))
  })
  
  output$network <- renderVisNetwork({
    req(graph_data$nodes, graph_data$edges)
    
    # Recalculate node sizes before rendering
    graph_data$nodes <- calculate_node_size(graph_data$nodes)
    
    # Retrieve the selected layout
    layout_choice <- input$layout_choice
    
    vis_net <- visNetwork(graph_data$nodes, graph_data$edges) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visEdges(arrows = "to") %>%
      visInteraction(zoomView = TRUE, dragView = TRUE, multiselect = TRUE) %>%
      visNodes(
        font = list(size = 20, vadjust = -30), # Position labels outside nodes
        labelHighlightBold = FALSE, # Disable bold highlight on hover
        scaling = list(label = list(enabled = FALSE)) # Disable automatic scaling
      )
    
    # Apply layout based on user choice
    if (layout_choice == "forceAtlas2Based") {
      vis_net <- vis_net %>%
        visPhysics(
          solver = "forceAtlas2Based",
          stabilization = TRUE,
          forceAtlas2Based = list(
            gravitationalConstant = -100,
            centralGravity = 0.01,
            springLength = 150,
            springConstant = 0.08,
            avoidOverlap = 0.2,
            damping = 0.4
          )
        )
    } else if (layout_choice == "barnesHut") {
      vis_net <- vis_net %>%
        visPhysics(
          solver = "barnesHut",
          stabilization = TRUE,
          barnesHut = list(
            gravitationalConstant = -20000,
            centralGravity = 0.3,
            springLength = 200,
            springConstant = 0.05,
            damping = 0.09
          )
        )
    } else if (layout_choice == "hierarchical") {
      vis_net <- vis_net %>%
        visHierarchicalLayout(
          direction = "UD",  # Up-Down direction
          levelSeparation = 150,
          nodeSpacing = 150,
          treeSpacing = 200,
          blockShifting = TRUE,
          edgeMinimization = TRUE,
          parentCentralization = TRUE
        )
    } else if (layout_choice == "circular") {
      vis_net <- vis_net %>%
        visIgraphLayout(layout = "layout_in_circle")
    }
    
    vis_net
  })
}
