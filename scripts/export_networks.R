#!/usr/bin/env Rscript
# Export Caltech36 and Enron networks to GEXF format

library(network)
library(R.matlab)
library(igraph)

# We'll use rgexf for GEXF export
if (!require("rgexf", quietly = TRUE)) {
  cat("Installing rgexf package...\n")
  install.packages("rgexf", repos = "https://cran.r-project.org")
}
library(rgexf)

network_output_dir <- file.path("data", "networks")
dir.create(network_output_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# CALTECH36
# =============================================================================

cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Converting Caltech36 to GEXF\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# Find Caltech36 file
caltech_candidates <- c(
  file.path("data", "raw", "external", "caltech_36.mat")
)
caltech_file <- caltech_candidates[file.exists(caltech_candidates)][1]

if (is.na(caltech_file)) {
  stop("caltech_36.mat not found under ./data/raw/external/.")
}

cat("Loading Caltech36 from:", caltech_file, "\n")

# Load the MAT file
m <- R.matlab::readMat(caltech_file)
A <- as.matrix(m$A)

# Create network object
g <- network::network(A, directed = FALSE, matrix.type = "adjacency")

cat("  Nodes:", network.size(g), "\n")
cat("  Edges:", network.edgecount(g), "\n")

# Process attributes
if (!is.null(m$local.info)) {
  attr_df <- as.data.frame(m$local.info)
  
  # Expected order: [status, gender, major, major2, dorm/house, year, highschool]
  # Some files include an extra leading ID column
  if (ncol(attr_df) == 7) {
    names(attr_df) <- c("status", "gender", "major", "major2", "house", "year", "highschool")
  } else if (ncol(attr_df) == 8) {
    names(attr_df) <- c("id", "status", "gender", "major", "major2", "house", "year", "highschool")
  }
  
  # Push attributes into the network object
  for (col in names(attr_df)) {
    if (col != "id") {
      vals <- attr_df[[col]]
      # Replace 0 with NA (FB100 uses 0 for missing)
      vals[vals == 0] <- NA
      network::set.vertex.attribute(g, col, vals)
    }
  }
}

# Try individual attribute arrays
for (attr_name in c("gender", "major", "year", "dorm", "highschool")) {
  if (attr_name %in% names(m)) {
    vals <- as.vector(m[[attr_name]])
    vals[vals == 0] <- NA
    network::set.vertex.attribute(g, attr_name, vals)
  }
}

# Get node attributes
node_attrs <- network::list.vertex.attributes(g)
node_attrs <- node_attrs[!node_attrs %in% c("na", "vertex.names")]

cat("  Attributes:", paste(node_attrs, collapse = ", "), "\n")

# Get edge list
edges <- network::as.edgelist(g)
if (nrow(edges) > 0) {
  edges <- edges - 1  # rgexf uses 0-based indexing
}

# Get node data
n_nodes <- network.size(g)
node_ids <- 0:(n_nodes - 1)
node_labels <- paste0("Node_", 0:(n_nodes - 1))

# Build attributes data frame
attrs_list <- list()
for (attr in node_attrs) {
  vals <- network::get.vertex.attribute(g, attr)
  # Convert to character, handling NAs
  vals[is.na(vals)] <- "NA"
  vals <- as.character(vals)
  # Escape XML special characters
  vals <- gsub("&", "&amp;", vals, fixed = TRUE)
  vals <- gsub("<", "&lt;", vals, fixed = TRUE)
  vals <- gsub(">", "&gt;", vals, fixed = TRUE)
  vals <- gsub("\"", "&quot;", vals, fixed = TRUE)
  vals <- gsub("'", "&apos;", vals, fixed = TRUE)
  attrs_list[[attr]] <- vals
}

# Create attributes data frame
if (length(attrs_list) > 0) {
  attrs_df <- data.frame(attrs_list, stringsAsFactors = FALSE)
} else {
  attrs_df <- NULL
}

# Export to GEXF using rgexf
gexf_obj <- rgexf::gexf(
  nodes = data.frame(id = node_ids, label = node_labels, stringsAsFactors = FALSE),
  edges = if (nrow(edges) > 0) data.frame(source = edges[, 1], target = edges[, 2], stringsAsFactors = FALSE) else data.frame(),
  nodesAtt = attrs_df
)

caltech_output <- file.path(network_output_dir, "caltech_36.gexf")
rgexf::write.gexf(gexf_obj, output = caltech_output)

cat("✓ Saved", caltech_output, "\n\n")

# =============================================================================
# ENRON EMAILS
# =============================================================================

cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Converting Enron Emails to GEXF\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

if (!require("networkDynamicData", quietly = TRUE)) {
  stop("networkDynamicData package required. Install with: install.packages('networkDynamicData')")
}

if (!require("networkDynamic", quietly = TRUE)) {
  stop("networkDynamic package required. Install with: install.packages('networkDynamic')")
}

# Load and collapse network
data(enronEmails, package = "networkDynamicData")
enron_static <- networkDynamic::network.collapse(enronEmails, rule = "latest")

cat("  Nodes:", network.size(enron_static), "\n")
cat("  Edges:", network.edgecount(enron_static), "\n")
cat("  Directed:", network::is.directed(enron_static), "\n")

# Get vertex attributes
vertex_attrs <- network::list.vertex.attributes(enron_static)
vertex_attrs <- vertex_attrs[!vertex_attrs %in% c("na", "vertex.names")]

cat("  Attributes:", paste(vertex_attrs, collapse = ", "), "\n")

# Fix categorical NAs
fix_categorical_na <- function(attr_name, replacement = "Missing") {
  if (!(attr_name %in% vertex_attrs)) return(NULL)
  vals <- network::get.vertex.attribute(enron_static, attr_name)
  if (is.null(vals) || !length(vals)) return(NULL)
  if (is.list(vals)) vals <- unlist(vals, use.names = FALSE)
  if (is.factor(vals)) vals <- as.character(vals)
  if (is.character(vals) && any(is.na(vals))) {
    vals[is.na(vals)] <- replacement
    network::set.vertex.attribute(enron_static, attr_name, vals)
  }
  NULL
}

lapply(c("role", "dept", "person_name"), fix_categorical_na)

# Get edge list
edges <- network::as.edgelist(enron_static)
if (nrow(edges) > 0) {
  edges <- edges - 1  # rgexf uses 0-based indexing
}

# Get node data
n_nodes <- network.size(enron_static)
node_ids <- 0:(n_nodes - 1)
node_labels <- paste0("Node_", 0:(n_nodes - 1))

# Build attributes data frame
attrs_list <- list()
for (attr in vertex_attrs) {
  vals <- network::get.vertex.attribute(enron_static, attr)
  # Convert to character, handling NAs
  if (is.factor(vals)) vals <- as.character(vals)
  vals[is.na(vals)] <- "NA"
  vals <- as.character(vals)
  # Escape XML special characters
  vals <- gsub("&", "&amp;", vals, fixed = TRUE)
  vals <- gsub("<", "&lt;", vals, fixed = TRUE)
  vals <- gsub(">", "&gt;", vals, fixed = TRUE)
  vals <- gsub("\"", "&quot;", vals, fixed = TRUE)
  vals <- gsub("'", "&apos;", vals, fixed = TRUE)
  attrs_list[[attr]] <- vals
}

# Create attributes data frame
if (length(attrs_list) > 0) {
  attrs_df <- data.frame(attrs_list, stringsAsFactors = FALSE)
} else {
  attrs_df <- NULL
}

# Export to GEXF using rgexf
gexf_obj <- rgexf::gexf(
  nodes = data.frame(id = node_ids, label = node_labels, stringsAsFactors = FALSE),
  edges = if (nrow(edges) > 0) data.frame(source = edges[, 1], target = edges[, 2], stringsAsFactors = FALSE) else data.frame(),
  nodesAtt = attrs_df
)

enron_output <- file.path(network_output_dir, "enron_emails.gexf")
rgexf::write.gexf(gexf_obj, output = enron_output)

cat("✓ Saved", enron_output, "\n\n")

cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Conversion complete!\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
