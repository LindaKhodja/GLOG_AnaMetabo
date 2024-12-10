# utils.R
# Utility functions for handling SBML files, graph processing, and visualization

# Function to create an SBML file from graph data (nodes and edges)
save_sbml <- function(nodes, edges, file_path) {
  # Initialize the SBML document with version 1.2
  sbml_doc <- xml_new_root("sbml", version = "1.2", xmlns = "http://www.sbml.org/sbml/level2/version4")
  
  # Add the list of species (nodes)
  list_of_species <- xml_add_child(sbml_doc, "listOfSpecies")
  for (i in 1:nrow(nodes)) {
    # For each node, create a species entry in SBML
    species <- xml_add_child(list_of_species, "species",
                             id = nodes$id[i],
                             name = nodes$label[i])
  }
  
  # Add the list of reactions (edges)
  list_of_reactions <- xml_add_child(sbml_doc, "listOfReactions")
  for (i in 1:nrow(edges)) {
    # For each edge, create a reaction entry in SBML
    reaction <- xml_add_child(list_of_reactions, "reaction",
                              id = paste0("reaction_", i),
                              name = ifelse(!is.na(edges$label[i]), edges$label[i], paste0("reaction_", i)))
    # Add reactants (species)
    list_of_reactants <- xml_add_child(reaction, "listOfReactants")
    xml_add_child(list_of_reactants, "speciesReference", species = edges$from[i])
    # Add products (species)
    list_of_products <- xml_add_child(reaction, "listOfProducts")
    xml_add_child(list_of_products, "speciesReference", species = edges$to[i])
  }
  
  # Save the generated SBML document to the specified file path
  write_xml(sbml_doc, file_path)
}

# Function to calculate the node size based on its shape
calculate_node_size <- function(nodes) {
  # Assign node sizes based on shape
  nodes$size <- ifelse(
    nodes$shape == "square", 
    20,  # Size for square nodes (reactions)
    50   # Default size for other shapes (metabolites)
  )
  return(nodes)
}

# Define a custom color palette for compartments
compartment_colors <- c(
  "cytosol" = "darkred", 
  "extracellular region" = "purple",
  "endoplasmic reticulum membrane" = "darkorange", 
  "endoplasmic reticulum lumen" = "magenta",
  "Golgi membrane" = "gold", 
  "endocytic vesicle membrane" = "yellowgreen",
  "nucleoplasm" = "pink", 
  "endosome lumen" = "cyan", 
  "plasma membrane" = "skyblue", 
  "unknown" = "blue", 
  "nuclear envelope" = "darkgreen", 
  "mitochondrial inner membrane" = "darkslategray", 
  "mitochondrial intermembrane space" = "lightcoral" 
)


# Function to load and process the SBML file into graph data (nodes and edges)
load_sbml_data <- function(sbml_file) {
  # Read the SBML XML file
  doc <- read_xml(sbml_file)
  
  # Define namespaces for SBML XML parsing
  ns <- xml_ns_rename(xml_ns(doc), d1 = "sbml")
  
  # Extract the species (nodes) from the SBML file
  species_nodes <- xml_find_all(doc, ".//sbml:listOfSpecies/sbml:species", ns)
  nodes <- data.frame(
    id = xml_attr(species_nodes, "id"),  # Node ID (species ID)
    label = xml_attr(species_nodes, "name"),  # Label (species name)
    shape = "dot",  # Default shape is a circle for metabolites
    color.background = "lightgrey",  # Default background color
    size = 50,  # Default node size
    font.size = 20,  # Font size for labels
    stringsAsFactors = FALSE
  )
  
  # Check the SBO term for each node and modify the shape accordingly
  sbo_terms <- xml_attr(species_nodes, "sboTerm")
  
  # Modify shapes for specific SBO terms (e.g., "RNA", "gene", "protein")
  nodes$shape <- ifelse(sbo_terms == "SBO:0000278", "diamond", nodes$shape)  # For RNA
  nodes$shape <- ifelse(sbo_terms == "SBO:0000252", "dot", nodes$shape)      # For gene
  nodes$shape <- ifelse(sbo_terms == "SBO:000013", "triangleDown", nodes$shape) # For proteins
  
  # Extract the compartment from the node label (e.g., "name [compartment]")
  nodes$compartment <- ifelse(
    grepl("\\[.*\\]$", nodes$label),
    gsub(".*\\[(.*)\\]$", "\\1", nodes$label),
    "unknown"  # If no compartment info, set as "unknown"
  )
  
  # Assign colors based on the compartment (using predefined color palette)
  nodes$color.border <- ifelse(
    nodes$compartment %in% names(compartment_colors),
    compartment_colors[nodes$compartment],
    "black"  # Default border color for unknown compartments
  )
  
  # Clean up the labels by removing compartment info (e.g., "name [compartment]" becomes "name")
  nodes$label <- gsub("\\[.*\\]$", "", nodes$label)
  nodes$label <- trimws(nodes$label)  # Remove any leading or trailing whitespace
  
  # Extract the reactions (edges) from the SBML file
  reaction_nodes <- xml_find_all(doc, ".//sbml:listOfReactions/sbml:reaction", ns)
  
  # Add reaction nodes (squared red nodes for reactions)
  for (reaction in reaction_nodes) {
    reaction_id <- xml_attr(reaction, "id")
    reaction_name <- xml_attr(reaction, "name")
    
    # Define new reaction node with square shape and red background
    new_node <- data.frame(
      id = reaction_id,
      label = ifelse(!is.na(reaction_name), reaction_name, reaction_id),
      shape = "square",  # Reaction nodes are squares
      color.background = "red",  # Red color for reactions
      size = 10,  # Smaller size for reactions
      font.size = 20,  # Font size for reaction labels
      color.border = "black",  # Default border color
      compartment = "reaction",  # Default compartment for reactions
      stringsAsFactors = FALSE
    )
    nodes <- rbind(nodes, new_node)  # Add new reaction node to the nodes data frame
  }
  
  # Extract the links (edges) between species and reactions
  edges <- data.frame()  # Initialize empty data frame for edges
  
  for (reaction in reaction_nodes) {
    reaction_id <- xml_attr(reaction, "id")
    
    # Extract reactants (species involved in the reaction)
    reactants <- xml_find_all(reaction, ".//sbml:listOfReactants/sbml:speciesReference", ns)
    reactant_ids <- xml_attr(reactants, "species")
    for (reactant in reactant_ids) {
      edges <- rbind(edges, data.frame(
        from = reactant,  # Species (reactant)
        to = reaction_id,  # Reaction ID
        arrows = "to",  # Arrow direction (reactant -> reaction)
        dashes = FALSE,  # Solid line
        color = "black",  # Default color
        stringsAsFactors = FALSE
      ))
    }
    
    # Extract products (species produced by the reaction)
    products <- xml_find_all(reaction, ".//sbml:listOfProducts/sbml:speciesReference", ns)
    product_ids <- xml_attr(products, "species")
    for (product in product_ids) {
      edges <- rbind(edges, data.frame(
        from = reaction_id,  # Reaction ID
        to = product,  # Species (product)
        arrows = "to",  # Arrow direction (reaction -> product)
        dashes = FALSE,  # Solid line
        color = "black",  # Default color
        stringsAsFactors = FALSE
      ))
    }
    
    # Extract regulators (modulators of the reaction)
    modifiers <- xml_find_all(reaction, ".//sbml:listOfModifiers/sbml:modifierSpeciesReference", ns)
    modifier_ids <- xml_attr(modifiers, "species")
    for (modifier in modifiers) {
      sbo_term <- xml_attr(modifier, "sboTerm")  # Get the SBO term for regulation type
      
      # Assign color and dashes based on regulation type (positive, negative, etc.)
      if (!is.na(sbo_term)) {
        if (sbo_term == "SBO:0000459") {  # Positive regulation (activation)
          color <- "green"
          dashes <- TRUE
        } else if (sbo_term == "SBO:0000020") {  # Negative regulation (inhibition)
          color <- "red"
          dashes <- TRUE
        } else {
          color <- "black"
          dashes <- FALSE
        }
      } else {
        color <- "black"
        dashes <- FALSE
      }
      
      # Add the regulation edge to the edges data frame
      edges <- rbind(edges, data.frame(
        from = modifier_ids,  # Regulator species
        to = reaction_id,  # Reaction ID
        arrows = "to",  # Arrow direction (regulator -> reaction)
        dashes = dashes,  # Dotted or solid line
        color = color,  # Line color
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Return a list containing both nodes and edges data frames
  list(nodes = nodes, edges = edges)
}
