#!/usr/bin/env Rscript

cran_packages <- c(
  "concorR",
  "ergm",
  "ergm.count",
  "ergm.multi",
  "httr",
  "igraph",
  "intergraph",
  "jsonlite",
  "network",
  "networkDynamic",
  "networkDynamicData",
  "readxl",
  "R.matlab",
  "R.utils",
  "rgexf",
  "RSiena",
  "sna",
  "statnet"
)

missing <- cran_packages[!vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

message("FORGE R dependencies are installed.")
