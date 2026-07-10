#!/usr/bin/env Rscript
# =============================================================================
# DATASET DESCRIPTION AND DIAGNOSTICS REPORT
# =============================================================================
# Loads all benchmark networks, computes descriptive statistics and diagnostics,
# and writes a consolidated summary (CSV + RDS + JSON) for downstream analysis.

suppressPackageStartupMessages({
  library(network)
  library(igraph)
  library(jsonlite)
})

source_if_exists <- function(path) {
  if (file.exists(path)) {
    source(path, local = FALSE)
    return(TRUE)
  }
  FALSE
}

ensure_dataset_bundle <- function(bundle_path, results_dir) {
  if (file.exists(bundle_path)) {
    return(bundle_path)
  }

  cat("Bundle not found. Generating via load_all_datasets.R...\n")
  if (!file.exists("load_all_datasets.R")) {
    stop("load_all_datasets.R is required to generate the dataset bundle.")
  }

  status <- system("Rscript load_all_datasets.R", intern = FALSE)
  if (!file.exists(bundle_path)) {
    stop("Failed to generate all_benchmark_datasets.rds; check load_all_datasets.R output.")
  }
  bundle_path
}

network_to_igraph <- function(net) {
  mode <- ifelse(network::is.directed(net), "directed", "undirected")
  mat <- as.matrix.network(net, matrix.type = "adjacency")
  igraph::graph_from_adjacency_matrix(mat, mode = mode, diag = FALSE)
}

summarise_attributes <- function(net, attribute_names) {
  attrs <- setdiff(attribute_names, c("na", "vertex.names"))
  if (length(attrs) == 0) {
    return(data.frame())
  }
  do.call(rbind, lapply(attrs, function(attr_name) {
    values <- network::get.vertex.attribute(net, attr_name)
    data.frame(
      attribute = attr_name,
      type = class(values)[1],
      unique_values = length(unique(values)),
      missing = sum(is.na(values)),
      stringsAsFactors = FALSE
    )
  }))
}

compute_diagnostics <- function(net) {
  is_dir <- network::is.directed(net)
  node_count <- network.size(net)
  edge_count <- network.edgecount(net)
  density <- if (node_count <= 1) {
    0
  } else {
    denom <- if (is_dir) node_count * (node_count - 1) else (node_count * (node_count - 1)) / 2
    edge_count / denom
  }

  g <- network_to_igraph(net)

  degree_all <- igraph::degree(g, mode = "all")
  avg_degree <- if (length(degree_all) > 0) mean(degree_all) else NA_real_
  degree_sd <- if (length(degree_all) > 1) sd(degree_all) else 0

  indegree_mean <- if (is_dir) mean(igraph::degree(g, mode = "in")) else NA_real_
  outdegree_mean <- if (is_dir) mean(igraph::degree(g, mode = "out")) else NA_real_
  reciprocity <- if (is_dir) tryCatch(igraph::reciprocity(g), error = function(e) NA_real_) else NA_real_
  transitivity <- tryCatch(igraph::transitivity(g, type = "global"), error = function(e) NA_real_)
  triangles <- if (!is_dir) {
    tri_counts <- igraph::count_triangles(g)
    sum(tri_counts) / 3
  } else {
    NA_real_
  }

  comp <- igraph::components(g, mode = if (is_dir) "weak" else "weak")
  components <- comp$no
  largest_component <- if (length(comp$csize) > 0) max(comp$csize) else NA_integer_

  if (node_count > 0 && node_count <= 500) {
    avg_path <- tryCatch(igraph::mean_distance(g, directed = is_dir, unconnected = TRUE), error = function(e) NA_real_)
    diam <- tryCatch(igraph::diameter(g, directed = is_dir, unconnected = TRUE), error = function(e) NA_real_)
    if (!is.finite(avg_path)) avg_path <- NA_real_
    if (!is.finite(diam)) diam <- NA_real_
  } else {
    avg_path <- NA_real_
    diam <- NA_real_
  }

  c(
    nodes = node_count,
    edges = edge_count,
    density = density,
    avg_degree = avg_degree,
    degree_sd = degree_sd,
    indegree_mean = indegree_mean,
    outdegree_mean = outdegree_mean,
    reciprocity = reciprocity,
    transitivity = transitivity,
    triangles = triangles,
    components = components,
    largest_component = largest_component,
    avg_path_length = avg_path,
    diameter = diam
  )
}

# -----------------------------------------------------------------------------
# Main execution
# -----------------------------------------------------------------------------

cat("📊 DATASET DESCRIPTION & DIAGNOSTICS\n")
cat("===================================\n")

results_dir <- "results"
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
}

bundle_path <- file.path(results_dir, "all_benchmark_datasets.rds")
bundle_path <- ensure_dataset_bundle(bundle_path, results_dir)
cat(sprintf("Using dataset bundle: %s\n\n", bundle_path))

datasets <- readRDS(bundle_path)
dataset_names <- names(datasets)

if (length(dataset_names) == 0) {
  stop("No datasets found in the bundle.")
}

diagnostic_rows <- list()
attribute_details <- list()

for (dataset_name in dataset_names) {
  cat(sprintf("🔍 %s\n", toupper(dataset_name)))
  cat(paste(rep("-", nchar(dataset_name) + 4), collapse = ""), "\n")

  entry <- datasets[[dataset_name]]
  net <- entry$network
  metadata <- entry$metadata
  attrs <- entry$attributes

  diagnostics <- compute_diagnostics(net)
  diagnostic_rows[[dataset_name]] <- c(list(dataset = dataset_name, directed = network::is.directed(net)), diagnostics)

  attribute_summary <- summarise_attributes(net, attrs)
  attribute_details[[dataset_name]] <- attribute_summary

  cat(sprintf("Name: %s\n", metadata$name))
  cat(sprintf("Description: %s\n", metadata$description))
  cat(sprintf("Nodes: %d | Edges: %d | Directed: %s\n",
              diagnostics["nodes"], diagnostics["edges"],
              ifelse(network::is.directed(net), "Yes", "No")))
  cat(sprintf("Density: %.4f | Avg degree: %.2f\n",
              diagnostics["density"], diagnostics["avg_degree"]))

  if (network::is.directed(net)) {
    cat(sprintf("Reciprocity: %.4f | Mean indegree: %.2f | Mean outdegree: %.2f\n",
                diagnostics["reciprocity"], diagnostics["indegree_mean"], diagnostics["outdegree_mean"]))
  }

  cat(sprintf("Transitivity: %.4f | Components: %d | Largest component: %d\n",
              diagnostics["transitivity"], diagnostics["components"], diagnostics["largest_component"]))

  if (!is.na(diagnostics["triangles"])) {
    cat(sprintf("Triangles: %.0f\n", diagnostics["triangles"]))
  }

  if (!is.na(diagnostics["avg_path_length"])) {
    cat(sprintf("Average path length: %.2f | Diameter: %.2f\n",
                diagnostics["avg_path_length"], diagnostics["diameter"]))
  } else {
    cat("Average path length / diameter: skipped (network too large or disconnected)\n")
  }

  meaningful_attrs <- setdiff(attrs, c("na", "vertex.names"))
  if (length(meaningful_attrs) > 0) {
    cat(sprintf("Attributes (%d): %s\n",
                length(meaningful_attrs), paste(meaningful_attrs, collapse = ", ")))
  } else {
    cat("Attributes: none\n")
  }

  cat("\n")
}

diagnostic_df <- do.call(rbind, lapply(diagnostic_rows, function(x) as.data.frame(as.list(x), stringsAsFactors = FALSE)))

numeric_cols <- setdiff(names(diagnostic_df), c("dataset", "directed"))
diagnostic_df[numeric_cols] <- lapply(diagnostic_df[numeric_cols], function(col) {
  suppressWarnings(as.numeric(col))
})

csv_path <- file.path(results_dir, "dataset_diagnostics_summary.csv")
rds_path <- file.path(results_dir, "dataset_diagnostics_summary.rds")
json_path <- file.path(results_dir, "dataset_diagnostics_summary.json")

write.csv(diagnostic_df, csv_path, row.names = FALSE)
saveRDS(list(summary = diagnostic_df, attributes = attribute_details), rds_path)

jsonlite::write_json(list(
  generated_at = Sys.time(),
  summary = diagnostic_df,
  attribute_details = attribute_details
), json_path, pretty = TRUE, auto_unbox = TRUE)

cat("📁 Outputs written:\n")
cat(sprintf("- %s\n", csv_path))
cat(sprintf("- %s\n", rds_path))
cat(sprintf("- %s\n", json_path))

cat("\n✅ Diagnostics complete.\n")
