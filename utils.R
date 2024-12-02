
# Fonction pour générer des couleurs dynamiques
generate_colors <- function(n) {
  max_colors <- 12
  if (n <= max_colors) {
    brewer.pal(n, "Set3")
  } else {
    colorRampPalette(brewer.pal(max_colors, "Set3"))(n)
  }
}

# Fonction pour créer un fichier SBML à partir des données du graphe
save_sbml <- function(nodes, edges, file_path) {
  sbml_doc <- xml_new_root("sbml", version = "1.2", xmlns = "http://www.sbml.org/sbml/level2/version4")
  
  # Ajouter la liste des espèces (nœuds)
  list_of_species <- xml_add_child(sbml_doc, "listOfSpecies")
  for (i in 1:nrow(nodes)) {
    species <- xml_add_child(list_of_species, "species",
                             id = nodes$id[i],
                             name = nodes$label[i])
  }
  
  # Ajouter la liste des réactions (arêtes)
  list_of_reactions <- xml_add_child(sbml_doc, "listOfReactions")
  for (i in 1:nrow(edges)) {
    reaction <- xml_add_child(list_of_reactions, "reaction",
                              id = paste0("reaction_", i),
                              name = ifelse(!is.na(edges$label[i]), edges$label[i], paste0("reaction_", i)))
    list_of_reactants <- xml_add_child(reaction, "listOfReactants")
    xml_add_child(list_of_reactants, "speciesReference", species = edges$from[i])
    list_of_products <- xml_add_child(reaction, "listOfProducts")
    xml_add_child(list_of_products, "speciesReference", species = edges$to[i])
  }
  
  # Sauvegarder le fichier
  write_xml(sbml_doc, file_path)
}

# Fonction pour charger et traiter le fichier SBML
load_sbml_data <- function(sbml_file) {
  doc <- read_xml(sbml_file)
  ns <- xml_ns_rename(xml_ns(doc), d1 = "sbml")
  
  # Extraire les métabolites (nœuds) sans les compartiments
  species_nodes <- xml_find_all(doc, ".//sbml:listOfSpecies/sbml:species", ns)
  nodes <- data.frame(
    id = xml_attr(species_nodes, "id"),
    label = xml_attr(species_nodes, "name"),
    shape = "dot",  # Mettre les espèces sous forme de cercle
    color = "blue", # Couleur par défaut des espèces
    stringsAsFactors = FALSE
  )
  
  # Extraire le compartiment à partir de l'ID
  nodes$group <- ifelse(
    grepl("\\[.*\\]$", nodes$label),
    gsub(".*\\[(.*)\\]$", "\\1", nodes$label),
    "unknown"
  )
  
  # Extraire les réactions (liens)
  reaction_nodes <- xml_find_all(doc, ".//sbml:listOfReactions/sbml:reaction", ns)
  
  # Ajouter les nœuds de réaction (carrés rouges)
  for (reaction in reaction_nodes) {
    reaction_id <- xml_attr(reaction, "id")
    reaction_name <- xml_attr(reaction, "name")
    
    new_node <- data.frame(
      id = reaction_id,
      label = ifelse(!is.na(reaction_name), reaction_name, reaction_id),
      shape = "square",  # Forme carrée pour les réactions
      color = "red",     # Couleur rouge pour les réactions
      group = "reaction",
      stringsAsFactors = FALSE
    )
    nodes <- rbind(nodes, new_node)
  }
  
  # Extraire les liens entre espèces et réactions (arêtes)
  edges <- data.frame()
  
  for (reaction in reaction_nodes) {
    reaction_id <- xml_attr(reaction, "id")
    
    # Réactifs
    reactants <- xml_find_all(reaction, ".//sbml:listOfReactants/sbml:speciesReference", ns)
    reactant_ids <- xml_attr(reactants, "species")
    for (reactant in reactant_ids) {
      edges <- rbind(edges, data.frame(
        from = reactant,
        to = reaction_id,
        label = "reactant",
        arrows = "to",
        dashes = FALSE,
        color = "black",
        stringsAsFactors = FALSE
      ))
    }
    
    # Produits
    products <- xml_find_all(reaction, ".//sbml:listOfProducts/sbml:speciesReference", ns)
    product_ids <- xml_attr(products, "species")
    for (product in product_ids) {
      edges <- rbind(edges, data.frame(
        from = reaction_id,
        to = product,
        label = "product",
        arrows = "to",
        dashes = FALSE,
        color = "black",
        stringsAsFactors = FALSE
      ))
    }
    
    # Régulateurs (modulateurs)
    modifiers <- xml_find_all(reaction, ".//sbml:listOfModifiers/sbml:modifierSpeciesReference", ns)
    modifier_ids <- xml_attr(modifiers, "species")
    for (modifier in modifiers) {
      sbo_term <- xml_attr(modifier, "sboTerm")  # Extraire le terme SBO pour déterminer le type de régulation
      
      # Déterminer la couleur et le type de ligne pour les régulations
      if (!is.na(sbo_term)) {
        if (sbo_term == "SBO:0000170") {  # Activation (positive)
          color <- "blue"
          dashes <- TRUE
        } else if (sbo_term == "SBO:0000169") {  # Inhibition (négative)
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
        from = modifier_ids,
        to = reaction_id,
        label = "modulator",
        arrows = "to",
        dashes = dashes,
        color = color,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  list(nodes = nodes, edges = edges)
}
