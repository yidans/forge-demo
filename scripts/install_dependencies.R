#!/usr/bin/env Rscript
# The local demo (demo/live) needs only network, ergm, and jsonlite.
# The remaining packages are used by the offline experiment scripts.

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
