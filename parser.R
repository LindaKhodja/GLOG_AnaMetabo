# parser.R

# Function to generate dynamic colors
# This function creates a color palette for visualizing data.
# If the number of requested colors (n) is less than or equal to 12, it uses "Set3" from RColorBrewer.
# If n exceeds 12, it interpolates additional colors using colorRampPalette.
generate_colors <- function(n) {
  max_colors <- 12
  if (n <= max_colors) {
    brewer.pal(n, "Set3")  # Use predefined color palette for up to 12 colors
  } else {
    colorRampPalette(brewer.pal(max_colors, "Set3"))(n)  # Generate additional colors dynamically
  }
}

# Function to create an SBML file from graph data
# This function takes nodes and edges data frames and saves them as an SBML file.
# SBML (Systems Biology Markup Language) is used for representing biochemical networks.
save_sbml <- function(nodes, edges, file_path) {
  # Create an SBML document with the specified version and namespace
  sbml_doc <- xml_new_root("sbml", version = "1.2", xmlns = "http://www.sbml.org/sbml/level2/version4")
  
  # Add the list of species (nodes) to the SBML document
  list_of_species <- xml_add_child(sbml_doc, "listOfSpecies")
  for (i in 1:nrow(nodes)) {
    xml_add_child(list_of_species, "species",
                  id = nodes$id[i],           # Unique identifier for the species
                  name = nodes$label[i])      # Name or label of the species
  }
  
  # Add the list of reactions (edges) to the SBML document
  list_of_reactions <- xml_add_child(sbml_doc, "listOfReactions")
  for (i in 1:nrow(edges)) {
    reaction <- xml_add_child(list_of_reactions, "reaction",
                              id = paste0("reaction_", i),                # Unique ID for each reaction
                              name = ifelse(!is.na(edges$label[i]), edges$label[i], paste0("reaction_", i))) # Name or default reaction ID
    # Add reactants (input species)
    list_of_reactants <- xml_add_child(reaction, "listOfReactants")
    xml_add_child(list_of_reactants, "speciesReference", species = edges$from[i])
    # Add products (output species)
    list_of_products <- xml_add_child(reaction, "listOfProducts")
    xml_add_child(list_of_products, "speciesReference", species = edges$to[i])
  }
  
  # Save the SBML document to the specified file path
  write_xml(sbml_doc, file_path)
}

# Function to load and process an SBML file
# This function parses an SBML file and extracts node and edge data for visualization.
load_sbml_data <- function(sbml_file) {
  # Read the SBML file
  doc <- read_xml(sbml_file)
  # Rename namespace to "sbml" for easier querying
  ns <- xml_ns_rename(xml_ns(doc), d1 = "sbml")
  
  # Extract species (nodes) from the SBML file
  species_nodes <- xml_find_all(doc, ".//sbml:listOfSpecies/sbml:species", ns)
  nodes <- data.frame(
    id = xml_attr(species_nodes, "id"),          # Unique ID for each species
    label = xml_attr(species_nodes, "name"),     # Name of the species
    shape = "dot",                               # Set shape to a circle for species nodes
    color = "blue",                              # Default color for species nodes
    stringsAsFactors = FALSE
  )
  
  # Extract compartment information from the species label
  nodes$group <- ifelse(
    grepl("\\[.*\\]$", nodes$label),             # Check if label contains compartment info (e.g., "[compartment]")
    gsub(".*\\[(.*)\\]$", "\\1", nodes$label),  # Extract compartment name
    "unknown"                                   # Default group if no compartment info is found
  )
  
  # Extract reactions (edges) from the SBML file
  reaction_nodes <- xml_find_all(doc, ".//sbml:listOfReactions/sbml:reaction", ns)
  
  # Add reaction nodes (square shapes, red color)
  for (reaction in reaction_nodes) {
    reaction_id <- xml_attr(reaction, "id")      # Unique ID for the reaction
    reaction_name <- xml_attr(reaction, "name") # Name of the reaction
    
    new_node <- data.frame(
      id = reaction_id,
      label = ifelse(!is.na(reaction_name), reaction_name, reaction_id), # Use name or ID as label
      shape = "square",                      # Square shape for reaction nodes
      color = "red",                         # Red color for reaction nodes
      group = "reaction",                    # Group as "reaction"
      stringsAsFactors = FALSE
    )
    nodes <- rbind(nodes, new_node)          # Append to the nodes data frame
  }
  
  # Initialize edges data frame
  edges <- data.frame()
  
  # Process reactants, products, and modifiers for each reaction
  for (reaction in reaction_nodes) {
    reaction_id <- xml_attr(reaction, "id")   # Reaction ID
    
    # Extract reactants (input species to reaction)
    reactants <- xml_find_all(reaction, ".//sbml:listOfReactants/sbml:speciesReference", ns)
    reactant_ids <- xml_attr(reactants, "species")
    for (reactant in reactant_ids) {
      edges <- rbind(edges, data.frame(
        from = reactant,                     # Edge from reactant to reaction
        to = reaction_id,
        label = "reactant",
        arrows = "to",                       # Directional edge
        dashes = FALSE,                      # Solid line
        color = "black",                     # Default edge color
        stringsAsFactors = FALSE
      ))
    }
    
    # Extract products (reaction to output species)
    products <- xml_find_all(reaction, ".//sbml:listOfProducts/sbml:speciesReference", ns)
    product_ids <- xml_attr(products, "species")
    for (product in product_ids) {
      edges <- rbind(edges, data.frame(
        from = reaction_id,                  # Edge from reaction to product
        to = product,
        label = "product",
        arrows = "to",
        dashes = FALSE,
        color = "black",
        stringsAsFactors = FALSE
      ))
    }
    
    # Extract modifiers (regulators of reactions)
    modifiers <- xml_find_all(reaction, ".//sbml:listOfModifiers/sbml:modifierSpeciesReference", ns)
    modifier_ids <- xml_attr(modifiers, "species")
    for (modifier in modifiers) {
      sbo_term <- xml_attr(modifier, "sboTerm") # SBO term for regulation type
      
      # Determine edge style based on SBO term
      if (!is.na(sbo_term)) {
        if (sbo_term == "SBO:0000170") {       # Activation
          color <- "blue"
          dashes <- TRUE
        } else if (sbo_term == "SBO:0000169") {# Inhibition
          color <- "orange"
          dashes <- TRUE
        } else {
          color <- "black"
          dashes <- FALSE
        }
      } else {
        color <- "black"
        dashes <- FALSE
      }
      
      edges <- rbind(edges, data.frame(
        from = modifier_ids,                 # Edge from modifier to reaction
        to = reaction_id,
        label = "modulator",
        arrows = "to",
        dashes = dashes,                     # Dashed line for regulation
        color = color,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Return the nodes and edges as a list
  list(nodes = nodes, edges = edges)
}
