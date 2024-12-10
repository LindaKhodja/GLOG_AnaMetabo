## Bioinformatics Network Visualization & Graph Analysis
This is an interactive web-based application built with RShiny that allows users to visualize and analyze biological networks. The app can handle SBML (Systems Biology Markup Language) files to create graphs, and provides features for adding/deleting nodes and edges, as well as performing various types of analysis on the network. These analyses include centrality analysis and cluster analysis, which help identify important nodes and communities within the network.

# Features
Graph Creation: Import an SBML file to create and visualize a biological network.
Add/Delete Nodes & Edges: Easily add or remove nodes and edges from the graph.
Node Customization: Customize node appearance based on shape, compartment, and color.
Graph Analysis:
Centrality Analysis: Evaluate the centrality of nodes using degree, closeness, or betweenness centrality metrics.
Cluster Analysis: Identify clusters of nodes within the graph using community detection algorithms like walktrap.
Graph Visualization: Visualize the network with interactive features such as zooming, panning, and node highlighting.

# Installation
To run this application locally, follow the steps below.

# Prerequisites
R (version 4.x or higher)
RStudio (optional, but recommended)
Shiny package: For building web applications.
visNetwork package: For rendering interactive graphs.
igraph package: For graph manipulation and analysis.
memoise package: For caching and optimizing file loading.
DT package: For displaying analysis results in tables.
Install the necessary packages by running the following R code:


install.packages(c("shiny", "visNetwork", "igraph", "memoise", "DT"))

# Running the Application
Clone this repository to your local machine and navigate to the project directory. Then run the following command to start the app:

shiny::runApp("path_to_project_directory")
This will launch the app in your default web browser.

SBML File
You will need to provide an SBML file (e.g., H5N1_infection.sbml) to load the network data. You can download a sample SBML file from the app interface or upload your own file for analysis.

# Usage
Interface
Graph Creation:

Click the "Upload SBML File" button to load your SBML file.
The graph will be automatically displayed.
Add Nodes & Edges:

Use the "Add Node" button to create new nodes with customizable properties.
Use the "Add Edge" button to add edges between nodes, specifying the type of relationship (e.g., activation, inhibition).
Node Deletion:

Select a node and click the "Delete Node" button to remove it from the graph.
Graph Analysis:

Perform centrality analysis by selecting the centrality metric (degree, closeness, or betweenness).
Perform cluster analysis to detect clusters of nodes within the network.
Download SBML:

Download the current network as an SBML file to save your work.

# Advanced Features
Centrality Metrics: Degree, closeness, and betweenness centrality are available to evaluate the importance of nodes within the network.
Community Detection: Use the walktrap algorithm to identify clusters of nodes that are tightly connected.
Interactive Graph: Zoom, drag, and interact with the network graph for better visualization and analysis.
Additions to the App
To further enhance the functionality of the app, consider the following suggestions:

1. Data Export
CSV Export: Allow users to export nodes and edges as CSV files for further analysis or reporting.
Image Export: Enable users to export the current graph as an image (e.g., PNG or SVG).
2. Network Layout Options
Allow users to choose different layout algorithms for visualizing the graph (e.g., force-directed, circular, hierarchical).
3. More Graph Analysis
Pathway Analysis: Add an option to find the shortest path between two nodes or analyze path lengths.
Graph Metrics: Include additional graph metrics such as density, diameter, or average path length.
4. Advanced Node Attributes
Enable users to add custom attributes to nodes (e.g., expression levels, types, or biological properties) and color nodes based on these attributes.
5. User Authentication
For collaborative environments, implement a user authentication system so that users can save their work or share graphs with others.
6. Interactive Tutorials
Provide an interactive tutorial to guide new users through the main features of the app, such as loading SBML files, adding nodes/edges, and performing analyses.
7. Custom Graph Analytics
Implement more advanced graph analytics features such as community detection algorithms, or advanced centrality measures like eigenvector centrality or PageRank.
