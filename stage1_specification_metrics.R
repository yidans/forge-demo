#!/usr/bin/env Rscript
# Stage I Specification Metrics
# Augments stored specifications with diagnostic metrics and produces summary statistics.

suppressPackageStartupMessages({
  library(network)
  library(ergm)
  library(jsonlite)
})

source("stage1_candidate_library.R")
source("benchmark_datasets.R")

`%||%` <- function(x, y) if (!is.null(x)) x else y

canonicalize_term <- function(s) {
  s <- gsub("\\s+", "", s)
  s <- sub("^\\+", "", s)
  s <- gsub("=true\\)", "=TRUE)", s, ignore.case = TRUE)
  s <- gsub("=false\\)", "=FALSE)", s, ignore.case = TRUE)
  if (grepl("^gw(esp|dsp|degree|idegree|odegree|b1degree|b2degree)\\(", s)) {
    fn_match <- regexpr("^gw(esp|dsp|degree|idegree|odegree|b1degree|b2degree)", s)
    fn_name <- regmatches(s, fn_match)
    args_match <- regexpr("\\(.*\\)", s)
    args_str <- regmatches(s, args_match)
    args_str <- gsub("[()]", "", args_str)
    parts <- strsplit(args_str, ",")[[1]]
    kv <- setNames(sub(".*=", "", parts), tolower(sub("=.*", "", parts)))
    decay <- kv[["decay"]]
    fixed <- kv[["fixed"]]
    if (is.na(fixed)) fixed <- "TRUE"
    s <- sprintf("%s(decay=%s,fixed=%s)", fn_name, decay, fixed)
  }
  s
}

canonicalize_vec <- function(v) {
  if (length(v) == 0) return(character(0))
  unique(vapply(v, canonicalize_term, character(1), USE.NAMES = FALSE))
}

normalize_formula_terms <- function(terms, attr_names) {
  if (is.null(terms) || length(terms) == 0) return(character(0))
  attr_lower <- tolower(attr_names)
  normalize_single <- function(term) {
    t <- trimws(term)
    t <- gsub("\\s+", "", t)
    t <- sub("^\\+", "", t)
    pattern <- '^([a-z_]+)\\((.*)\\)$'
    if (!grepl(pattern, t, ignore.case = TRUE)) return(t)
    matches <- regexec(pattern, t, ignore.case = TRUE)
    parts <- regmatches(t, matches)[[1]]
    if (length(parts) < 3) return(t)
    fn <- tolower(parts[2])
    args <- parts[3]
    attr_functions <- c("nodematch", "nodemix", "nodefactor", "nodeifactor", "nodeofactor",
                        "nodecov", "nodeicov", "nodeocov", "absdiff", "edgecov")
    if (!(fn %in% attr_functions)) return(t)
    if (grepl("=", args)) return(t)
    arg_clean <- gsub("^['\"]|['\"]$", "", args)
    match_idx <- match(tolower(arg_clean), attr_lower)
    canonical <- if (!is.na(match_idx)) attr_names[match_idx] else arg_clean
    sprintf("%s(\"%s\")", fn, canonical)
  }
  vapply(terms, normalize_single, character(1), USE.NAMES = FALSE)
}

get_term_function <- function(term) sub("\\(.*", "", term)

extract_attribute_from_term <- function(term) {
  pattern <- "^([a-z_]+)\\(\\\"([^\\\"]+)\\\".*\\)$"
  if (!grepl(pattern, term, ignore.case = TRUE)) return(NA_character_)
  matches <- regexec(pattern, term, ignore.case = TRUE)
  parts <- regmatches(term, matches)[[1]]
  if (length(parts) < 3) return(NA_character_)
  parts[3]
}

starts_with_any <- function(x, prefixes) {
  any(vapply(prefixes, function(p) any(startsWith(tolower(x), p)), logical(1)))
}

logistic <- function(x) 1 / (1 + exp(-x))

# -----------------------------------------------------------------------------
# Metric calculations
# -----------------------------------------------------------------------------

compute_expected_effects <- function(terms, effects) {
  non_edges <- setdiff(terms, "edges")
  if (length(non_edges) == 0) {
    return(list(coverage = 1, missing = character(0), invalid = character(0)))
  }
  effect_names <- names(effects)
  if (is.null(effect_names)) effect_names <- character(0)
  canonical_effects <- canonicalize_vec(effect_names)
  values <- unname(unlist(effects))
  invalid_idx <- which(!values %in% c("+", "-"))
  invalid <- effect_names[invalid_idx]
  missing <- setdiff(non_edges, canonical_effects)
  coverage <- if (length(non_edges) == 0) 1 else (length(non_edges) - length(missing)) / length(non_edges)
  coverage <- max(min(coverage, 1), 0)
  list(coverage = coverage, missing = missing, invalid = effect_names[invalid_idx])
}

compute_diag_alignment <- function(terms, attr_meta, diags) {
  score <- 0
  rationale <- character(0)
  closure_needed <- (!is.null(diags$clustering) && !is.na(diags$clustering) && diags$clustering >= 0.2) ||
                    (!is.null(diags$triangles) && !is.na(diags$triangles) && diags$triangles > 0)
  if (closure_needed && starts_with_any(terms, c("gwesp(", "gwdsp("))) {
    score <- score + 1
    rationale <- c(rationale, "closure")
  }
  heavy_tail <- FALSE
  if (!is.null(diags$degree_quantiles) && !any(is.na(diags$degree_quantiles))) {
    dq <- diags$degree_quantiles
    if (length(dq) >= 5) heavy_tail <- dq[5] >= dq[4] + 2 && dq[5] >= 4
  }
  if (heavy_tail && starts_with_any(terms, c("gwdegree(", "gwidegree(", "gwodegree(", "gwb1degree(", "gwb2degree("))) {
    score <- score + 1
    rationale <- c(rationale, "degree_shape")
  }
  attr_present <- FALSE
  attr_functions <- c("nodematch", "nodemix", "nodefactor", "nodeifactor", "nodeofactor", "nodecov", "nodeicov", "nodeocov", "absdiff")
  for (term in terms) {
    fn <- tolower(get_term_function(term))
    if (fn %in% attr_functions) {
      attr <- extract_attribute_from_term(term)
      if (!is.na(attr) && !is.null(attr_meta[[attr]])) {
        attr_present <- TRUE
        break
      }
    }
  }
  if (attr_present) {
    score <- score + 1
    rationale <- c(rationale, "attribute_term")
  }
  list(score = score, rationale = rationale)
}

compute_redundancy <- function(terms) {
  redundancies <- character(0)
  if (starts_with_any(terms, c("gwdegree(")) && starts_with_any(terms, c("kstar("))) {
    redundancies <- c(redundancies, "gwdegree with kstar")
  }
  attr_terms <- vapply(terms, extract_attribute_from_term, character(1))
  for (attr in unique(attr_terms[!is.na(attr_terms)])) {
    if (attr == "") next
    has_nodemix <- any(grepl(sprintf('^nodemix\\(\"%s\"', attr), terms))
    has_nodefactor <- any(grepl(sprintf('^nodefactor\\(\"%s\"', attr), terms))
    if (has_nodemix && has_nodefactor) {
      redundancies <- c(redundancies, sprintf("nodemix and nodefactor for %s", attr))
    }
  }
  redundancies
}

compute_type_violations <- function(terms, attr_meta) {
  attr_functions <- c("nodematch", "nodemix", "nodefactor", "nodeifactor", "nodeofactor",
                      "nodecov", "nodeicov", "nodeocov", "absdiff")
  cat_on_num <- 0
  num_on_cat <- 0
  for (term in terms) {
    fn <- tolower(get_term_function(term))
    if (fn %in% attr_functions) {
      attr <- extract_attribute_from_term(term)
      if (is.na(attr)) next
      info <- attr_meta[[attr]]
      if (is.null(info)) next
      if (fn %in% c("nodecov", "nodeicov", "nodeocov", "absdiff") && info$classification != "numeric") {
        num_on_cat <- num_on_cat + 1
      }
      if (fn %in% c("nodematch", "nodemix", "nodefactor", "nodeifactor", "nodeofactor") && info$classification != "categorical") {
        cat_on_num <- cat_on_num + 1
      }
    }
  }
  list(categorical_on_numeric = cat_on_num, numeric_on_categorical = num_on_cat)
}

compute_mple_delta <- function(net, formula_terms) {
  if (length(formula_terms) == 0) return(NA_real_)
  full_formula <- paste("edges", paste(formula_terms[formula_terms != "edges"], collapse = " + "), sep = if (length(formula_terms) > 1) " + " else "")
  spec_formula <- as.formula(paste("net ~", full_formula))
  edges_formula <- net ~ edges
  pl_aic_edges <- pl_aic_spec <- NA
  try({
    fit_edges <- ergm(edges_formula, estimate = "MPLE", control = control.ergm(MCMLE.maxit = 0))
    pl_aic_edges <- AIC(fit_edges)
  }, silent = TRUE)
  try({
    fit_spec <- ergm(spec_formula, estimate = "MPLE", control = control.ergm(MCMLE.maxit = 0))
    pl_aic_spec <- AIC(fit_spec)
  }, silent = TRUE)
  if (is.na(pl_aic_edges) || is.na(pl_aic_spec)) return(NA_real_)
  pl_aic_edges - pl_aic_spec
}

compute_forge_score <- function(metrics) {
  score <- 0
  if (isTRUE(metrics$library_compliance)) score <- score + 20
  score <- score + 20 * metrics$expected_effects_coverage
  score <- score + 15 * (metrics$diag_alignment / 3)
  if (metrics$structural_coverage == 1) score <- score + 15
  if (metrics$type_violations == 0) score <- score + 10
  pl_delta <- metrics$pl_aic_delta
  if (!is.na(pl_delta)) score <- score + 20 * logistic(pl_delta / 10)
  if (length(metrics$redundancy) > 0) score <- score - 10
  score
}

# -----------------------------------------------------------------------------
# Main execution
# -----------------------------------------------------------------------------

input_path <- "results/stage1_specifications_results.rds"
if (!file.exists(input_path)) {
  stop("results/stage1_specifications_results.rds not found. Run specification generation first.")
}

spec_results <- readRDS(input_path)
metrics_results <- spec_results
summary_rows <- list()

for (dataset_name in names(spec_results)) {
  dataset_entry <- spec_results[[dataset_name]]
  admissible_terms <- dataset_entry$admissible_library
  attr_meta <- dataset_entry$attribute_details
  diagnostics <- dataset_entry$diagnostics
  attr_names <- names(attr_meta)

  ds <- load_benchmark_dataset(dataset_name)
  net <- ds$network

  for (model_name in names(dataset_entry$models)) {
    model_entry <- dataset_entry$models[[model_name]]
    for (strategy_name in names(model_entry)) {
      spec_entry <- model_entry[[strategy_name]]
      metrics <- list()
      if (isTRUE(spec_entry$success)) {
        formula_source <- spec_entry$formula_raw %||% spec_entry$formula_normalized %||% character(0)
        normalized_formula <- normalize_formula_terms(formula_source, attr_names)
        canonical_formula <- canonicalize_vec(normalized_formula)
        library_norm <- canonicalize_vec(admissible_terms)
        metrics$library_compliance <- as.integer(all(canonical_formula %in% library_norm))
        ee <- compute_expected_effects(canonical_formula, spec_entry$expected_effects)
        metrics$expected_effects_coverage <- ee$coverage
        metrics$expected_effects_missing <- ee$missing
        metrics$expected_effects_invalid <- ee$invalid
        diag_align <- compute_diag_alignment(canonical_formula, attr_meta, diagnostics)
        metrics$diag_alignment <- diag_align$score
        metrics$diag_alignment_rationale <- diag_align$rationale
        metrics$size <- length(canonical_formula)
        redundancies <- compute_redundancy(canonical_formula)
        metrics$redundancy <- redundancies
        metrics$structural_coverage <- as.integer(starts_with_any(canonical_formula, c("gw", "mutual", "twopath")))
        type_violations <- compute_type_violations(canonical_formula, attr_meta)
        metrics$type_violations <- type_violations$categorical_on_numeric + type_violations$numeric_on_categorical
        if (!is.null(spec_entry$formula_normalized)) {
          metrics$formula_normalized <- normalized_formula
          metrics$formula_canonical <- canonical_formula
        }
        pl_delta <- compute_mple_delta(net, canonical_formula)
        metrics$pl_aic_delta <- pl_delta
        metrics$forge_stage1_score <- compute_forge_score(metrics)
      } else {
        metrics$library_compliance <- NA
        metrics$expected_effects_coverage <- NA
        metrics$diag_alignment <- NA
        metrics$diag_alignment_rationale <- character(0)
        metrics$size <- NA
        metrics$redundancy <- character(0)
        metrics$structural_coverage <- NA
        metrics$type_violations <- NA
        metrics$pl_aic_delta <- NA
        metrics$forge_stage1_score <- NA
      }
      metrics_results[[dataset_name]]$models[[model_name]][[strategy_name]]$metrics <- metrics
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        dataset = dataset_name,
        model = model_name,
        strategy = strategy_name,
        library_compliance = metrics$library_compliance,
        expected_effects_coverage = metrics$expected_effects_coverage,
        diag_alignment = metrics$diag_alignment,
        size = metrics$size,
        structural_coverage = metrics$structural_coverage,
        type_violations = metrics$type_violations,
        pl_aic_delta = metrics$pl_aic_delta,
        forge_stage1_score = metrics$forge_stage1_score,
        stringsAsFactors = FALSE
      )
    }
  }
}

summary_df <- do.call(rbind, summary_rows)
summary_table <- aggregate(. ~ model + strategy, data = summary_df[, !(names(summary_df) %in% "dataset")], FUN = function(x) mean(x, na.rm = TRUE))

output_rds <- "results/stage1_specifications_results_metrics.rds"
output_json <- "results/stage1_specifications_results_metrics.json"
output_csv <- "results/stage1_specifications_metrics_summary.csv"

saveRDS(metrics_results, output_rds)
write_json(metrics_results, output_json, auto_unbox = TRUE, pretty = TRUE)
write.csv(summary_table, output_csv, row.names = FALSE)

cat("Specification metrics complete. Outputs:\n")
cat(sprintf("- %s\n", output_rds))
cat(sprintf("- %s\n", output_json))
cat(sprintf("- %s\n", output_csv))
