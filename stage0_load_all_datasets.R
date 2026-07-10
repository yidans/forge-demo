#!/usr/bin/env Rscript
# Clean Dataset Loader - Load All Benchmark Datasets
# Comprehensive script to load all available datasets with proper error handling

library(ergm)
library(network)
library(statnet)
library(R.matlab)

# Source the benchmark datasets framework
source("benchmark_datasets.R")

# =============================================================================
# CORRECTED CALTECH36 LOADING
# =============================================================================

load_caltech36_corrected <- function() {
  cat("📊 Loading Caltech36 (corrected approach)...\n")
  
  caltech_candidates <- c(
    file.path("data", "raw", "external", "caltech_36.mat")
  )
  caltech_file <- caltech_candidates[file.exists(caltech_candidates)][1]
  if (is.na(caltech_file)) {
    stop("caltech_36.mat not found under ./data/raw/external/.")
  }
  
  # Load the MAT file
  m <- R.matlab::readMat(caltech_file)
  A <- as.matrix(m$A)
  g <- network::network(A, directed = FALSE, matrix.type = "adjacency")
  
  # Process attributes
  attr_df <- as.data.frame(m$local.info)
  
  # Expected order from the release: [status, gender, major, major2, dorm/house, year, highschool]
  # Some files include an extra leading ID column.
  if (ncol(attr_df) == 7) {
    names(attr_df) <- c("status","gender","major","major2","house","year","highschool")
  } else if (ncol(attr_df) == 8) {
    names(attr_df) <- c("id","status","gender","major","major2","house","year","highschool")
  } else {
    stop(sprintf("Unexpected number of columns in local.info: %d", ncol(attr_df)))
  }
  
  # Push attributes into the network object
  for (col in names(attr_df)) {
    network::set.vertex.attribute(g, col, attr_df[[col]])
  }
  
  # Create metadata
  meaningful_attrs <- list.vertex.attributes(g)[!list.vertex.attributes(g) %in% c("na", "vertex.names")]
  
  metadata <- list(
    name = "Caltech 36 Facebook Network",
    description = "Facebook friendship network at Caltech (Facebook100 dataset)",
    nodes = network.size(g),
    edges = network.edgecount(g),
    directed = FALSE,
    attributes = meaningful_attrs,
    context = "University Facebook friendship network with demographic and academic attributes",
    literature = c(
      "Traud et al. (2012). Social Networks",
      "Facebook100 dataset: https://archive.org/details/oxford-2005-facebook-matrix"
    )
  )
  
  return(list(
    network = g,
    metadata = metadata,
    attributes = list.vertex.attributes(g),
    node_count = network.size(g),
    edge_count = network.edgecount(g),
    is_directed = FALSE,
    density = network.edgecount(g) / (network.size(g) * (network.size(g) - 1) / 2)
  ))
}

# =============================================================================
# DATASET LOADING FUNCTIONS
# =============================================================================

load_all_benchmark_datasets <- function(save_dir = "results") {
  cat("🌐 LOADING ALL BENCHMARK DATASETS\n")
  cat("=================================\n\n")
  
  # Define all available datasets
  available_datasets <- c(
    "faux_mesa",
    "faux_dixon", 
    "faux_magnolia",
    "kapferer",
    "lazega",
    "krackhardt",
    "glasgow_s50",
    "manufacturing_emails",
    "enron_emails",
    "florentine",
    "noordin_top",
    "caltech_36"
  )
  
  datasets <- list()
  successful_loads <- 0
  failed_loads <- 0

  # Ensure results directory exists
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  for (dataset_name in available_datasets) {
    cat(sprintf("📊 Loading %s...\n", toupper(dataset_name)))
    
    tryCatch({
      # Use corrected approach for Caltech36
      if (dataset_name == "caltech_36") {
        dataset_result <- load_caltech36_corrected()
      } else {
        # Load other datasets using the standard approach
        dataset_result <- load_benchmark_dataset(dataset_name)
      }
      
      # Extract network and metadata
      network_obj <- dataset_result$network
      metadata <- dataset_result$metadata
      
      # Store in datasets list
      datasets[[dataset_name]] <- list(
        network = network_obj,
        metadata = metadata,
        attributes = list.vertex.attributes(network_obj),
        node_count = network.size(network_obj),
        edge_count = network.edgecount(network_obj),
        is_directed = is.directed(network_obj),
        density = network.edgecount(network_obj) / (network.size(network_obj) * (network.size(network_obj) - 1) / 2)
      )
      
      # Print summary
      cat(sprintf("  ✅ %s: %d nodes, %d edges, %s, density=%.4f\n", 
                 metadata$name,
                 metadata$nodes,
                 metadata$edges,
                 ifelse(metadata$directed, "directed", "undirected"),
                 datasets[[dataset_name]]$density))
      
      # Print attributes
      meaningful_attrs <- datasets[[dataset_name]]$attributes[!datasets[[dataset_name]]$attributes %in% c("na", "vertex.names")]
      if (length(meaningful_attrs) > 0) {
        cat(sprintf("     Attributes: %s\n", paste(meaningful_attrs, collapse = ", ")))
      } else {
        cat("     Attributes: none\n")
      }
      
      successful_loads <- successful_loads + 1
      
    }, error = function(e) {
      cat(sprintf("  ❌ Failed to load %s: %s\n", dataset_name, e$message))
      failed_loads <- failed_loads + 1
    })
    
    cat("\n")
  }
  
  # Summary
  cat("📈 LOADING SUMMARY\n")
  cat("=================\n")
  cat(sprintf("Total datasets attempted: %d\n", length(available_datasets)))
  cat(sprintf("Successfully loaded: %d\n", successful_loads))
  cat(sprintf("Failed to load: %d\n", failed_loads))
  cat(sprintf("Success rate: %.1f%%\n", (successful_loads / length(available_datasets)) * 100))
  
  return(datasets)
}

# =============================================================================
# DATASET EXPLORATION FUNCTIONS
# =============================================================================

explore_dataset <- function(dataset_name, datasets) {
  if (!dataset_name %in% names(datasets)) {
    cat(sprintf("❌ Dataset '%s' not found in loaded datasets\n", dataset_name))
    return(NULL)
  }
  
  dataset <- datasets[[dataset_name]]
  network_obj <- dataset$network
  metadata <- dataset$metadata
  
  cat(sprintf("🔍 EXPLORING %s\n", toupper(dataset_name)))
  cat(paste(rep("=", nchar(dataset_name) + 10), collapse=""), "\n")
  
  # Basic network properties
  cat(sprintf("Name: %s\n", metadata$name))
  cat(sprintf("Description: %s\n", metadata$description))
  cat(sprintf("Nodes: %d\n", dataset$node_count))
  cat(sprintf("Edges: %d\n", dataset$edge_count))
  cat(sprintf("Directed: %s\n", ifelse(dataset$is_directed, "Yes", "No")))
  cat(sprintf("Density: %.4f\n", dataset$density))
  
  # Attributes
  meaningful_attrs <- dataset$attributes[!dataset$attributes %in% c("na", "vertex.names")]
  if (length(meaningful_attrs) > 0) {
    cat(sprintf("Attributes: %s\n", paste(meaningful_attrs, collapse = ", ")))
    
    # Explore each attribute
    for (attr in meaningful_attrs) {
      attr_values <- get.vertex.attribute(network_obj, attr)
      unique_vals <- length(unique(attr_values))
      attr_type <- class(attr_values)[1]
      
      cat(sprintf("  %s: %s, %d unique values\n", attr, attr_type, unique_vals))
      
      # Show unique values for categorical attributes
      if (unique_vals <= 10) {
        unique_values <- sort(unique(attr_values))
        cat(sprintf("    Values: %s\n", paste(unique_values, collapse = ", ")))
      }
    }
  } else {
    cat("Attributes: none\n")
  }
  
  # Literature references
  if (!is.null(metadata$literature) && length(metadata$literature) > 0) {
    cat("Literature:\n")
    for (ref in metadata$literature) {
      cat(sprintf("  • %s\n", ref))
    }
  }
  
  cat("\n")
}

# =============================================================================
# DATASET COMPARISON FUNCTIONS
# =============================================================================

compare_datasets <- function(datasets) {
  cat("📊 DATASET COMPARISON\n")
  cat("===================\n\n")
  
  # Create comparison table
  comparison_data <- data.frame(
    Dataset = character(),
    Nodes = integer(),
    Edges = integer(),
    Directed = logical(),
    Density = numeric(),
    Attributes = integer(),
    stringsAsFactors = FALSE
  )
  
  for (dataset_name in names(datasets)) {
    dataset <- datasets[[dataset_name]]
    meaningful_attrs <- length(dataset$attributes[!dataset$attributes %in% c("na", "vertex.names")])
    
    comparison_data <- rbind(comparison_data, data.frame(
      Dataset = dataset_name,
      Nodes = dataset$node_count,
      Edges = dataset$edge_count,
      Directed = dataset$is_directed,
      Density = round(dataset$density, 4),
      Attributes = meaningful_attrs,
      stringsAsFactors = FALSE
    ))
  }
  
  # Print formatted table
  print(comparison_data, row.names = FALSE)
  
  cat("\n")
  
  # Summary statistics
  cat("📈 SUMMARY STATISTICS\n")
  cat("===================\n")
  cat(sprintf("Total datasets: %d\n", nrow(comparison_data)))
  cat(sprintf("Average nodes: %.1f (range: %d-%d)\n", 
             mean(comparison_data$Nodes), 
             min(comparison_data$Nodes), 
             max(comparison_data$Nodes)))
  cat(sprintf("Average edges: %.1f (range: %d-%d)\n", 
             mean(comparison_data$Edges), 
             min(comparison_data$Edges), 
             max(comparison_data$Edges)))
  cat(sprintf("Average density: %.4f (range: %.4f-%.4f)\n", 
             mean(comparison_data$Density), 
             min(comparison_data$Density), 
             max(comparison_data$Density)))
  cat(sprintf("Directed networks: %d/%d (%.1f%%)\n", 
             sum(comparison_data$Directed), 
             nrow(comparison_data),
             (sum(comparison_data$Directed) / nrow(comparison_data)) * 100))
  cat(sprintf("Average attributes: %.1f (range: %d-%d)\n", 
             mean(comparison_data$Attributes), 
             min(comparison_data$Attributes), 
             max(comparison_data$Attributes)))
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

main <- function() {
  cat("🚀 BENCHMARK DATASETS LOADER\n")
  cat("============================\n\n")
  save_dir <- "results"
  
  # Load all datasets
  datasets <- load_all_benchmark_datasets(save_dir = save_dir)
  
  if (length(datasets) > 0) {
    # Compare datasets
    compare_datasets(datasets)
    
    # Interactive exploration (optional)
    cat("🔍 EXPLORATION EXAMPLES\n")
    cat("======================\n")
    cat("To explore a specific dataset, use:\n")
    cat("  explore_dataset('faux_mesa', datasets)\n")
    cat("  explore_dataset('lazega', datasets)\n")
    cat("  explore_dataset('enron_emails', datasets)\n")
    cat("  explore_dataset('noordin_top', datasets)\n")
    cat("\n")
    
    # Show available datasets
    cat("📋 AVAILABLE DATASETS\n")
    cat("====================\n")
    for (dataset_name in names(datasets)) {
      cat(sprintf("• %s\n", dataset_name))
    }
    
  } else {
    cat("❌ No datasets were successfully loaded!\n")
  }
  
  return(datasets)
}

# Run main function if script is executed directly
if (!interactive()) {
  datasets <- main()
} else {
  cat("📚 Dataset loader functions loaded. Use main() to load all datasets.\n")
  cat("Available functions:\n")
  cat("  • load_all_benchmark_datasets() - Load all datasets\n")
  cat("  • explore_dataset(name, datasets) - Explore specific dataset\n")
  cat("  • compare_datasets(datasets) - Compare all datasets\n")
  cat("  • main() - Run complete loading and analysis\n")
}
